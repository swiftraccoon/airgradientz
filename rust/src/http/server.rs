use std::collections::HashMap;
use std::fs;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::{Duration, Instant};

use mio::net::TcpListener;
use mio::{Events, Interest, Poll, Token};
use crate::api;
use crate::error::AppError;
use crate::http::request::{HttpRequest, Method};
use crate::http::response::HttpResponse;
use crate::AppState;

const SERVER: Token = Token(0);
const MAX_READ_BUF: usize = 65_536;
const MAX_CONNECTIONS: usize = 1024;
const CONN_TIMEOUT: Duration = Duration::from_secs(30);
const POLL_TIMEOUT: Duration = Duration::from_secs(1);

enum ConnPhase {
    Reading,
    Writing,
}

struct ConnState {
    stream: mio::net::TcpStream,
    phase: ConnPhase,
    created_at: Instant,
    read_buf: Vec<u8>,
    response: Vec<u8>,
    write_pos: usize,
}

fn content_type_for(path: &Path) -> &'static str {
    match path.extension().and_then(|e| e.to_str()) {
        Some("html") => "text/html; charset=utf-8",
        Some("css") => "text/css; charset=utf-8",
        Some("js") => "application/javascript; charset=utf-8",
        Some("json") => "application/json; charset=utf-8",
        Some("png") => "image/png",
        Some("jpg" | "jpeg") => "image/jpeg",
        Some("svg") => "image/svg+xml",
        Some("ico") => "image/x-icon",
        _ => "application/octet-stream",
    }
}

fn serve_static(req_path: &str) -> HttpResponse {
    use crate::http::request::url_decode;

    // URL-decode the path
    let decoded = url_decode(req_path);

    // Check for path traversal in decoded path
    if decoded.contains("..") {
        return HttpResponse::not_found();
    }

    // Reject control characters (0x00-0x1F, 0x7F)
    if decoded.bytes().any(|b| b < 0x20 || b == 0x7f) {
        return HttpResponse::not_found();
    }

    let relative = decoded.trim_start_matches('/');
    let file_path = if relative.is_empty() {
        PathBuf::from("public/index.html")
    } else {
        PathBuf::from("public").join(relative)
    };

    // Resolve to absolute path and verify under public/
    let canonical = match fs::canonicalize(&file_path) {
        Ok(p) => p,
        Err(_) => return HttpResponse::not_found(),
    };
    let public_canonical = match fs::canonicalize("public") {
        Ok(p) => p,
        Err(_) => return HttpResponse::not_found(),
    };
    if !canonical.starts_with(&public_canonical) {
        return HttpResponse::not_found();
    }

    const MAX_STATIC_FILE: u64 = 16 * 1024 * 1024;

    let meta = match fs::metadata(&canonical) {
        Ok(m) if m.is_file() => m,
        _ => return HttpResponse::not_found(),
    };

    if meta.len() > MAX_STATIC_FILE {
        return HttpResponse::internal_error("File too large");
    }

    fs::read(&canonical).map_or_else(
        |_| HttpResponse::not_found(),
        |contents| {
            let ct = content_type_for(&canonical);
            HttpResponse::ok_static(contents, ct)
        },
    )
}

fn route_request(state: &AppState, req: &HttpRequest) -> HttpResponse {
    if req.method == Method::Get {
        match req.path.as_str() {
            "/api/readings" => api::handle_readings(state, req),
            "/api/readings/latest" => api::handle_readings_latest(state),
            "/api/devices" => api::handle_devices(state),
            "/api/health" => api::handle_health(state),
            "/api/config" => api::handle_config(state),
            "/api/stats" => api::handle_stats(state),
            path => serve_static(path),
        }
    } else {
        HttpResponse::method_not_allowed()
    }
}

/// Handle a readable event on a client connection.
/// Returns `true` if the connection should be removed.
fn handle_read(
    state: &AppState,
    conn: &mut ConnState,
    poll: &Poll,
    token: Token,
) -> bool {
    let mut tmp = [0u8; 4096];
    loop {
        match conn.stream.read(&mut tmp) {
            Ok(0) => {
                // EOF — client closed before sending a complete request
                return true;
            }
            Ok(n) => {
                conn.read_buf.extend_from_slice(&tmp[..n]);
                if conn.read_buf.len() > MAX_READ_BUF {
                    // Request too large — drop the connection
                    return true;
                }
            }
            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                break;
            }
            Err(_) => {
                return true;
            }
        }
    }

    // Check if we have a complete set of headers (double CRLF)
    if !contains_header_end(&conn.read_buf) {
        return false;
    }

    // We have a full request — parse and route
    let response = HttpRequest::parse_from_buf(&conn.read_buf).map_or_else(
        |_| HttpResponse::internal_error("Bad request"),
        |req| {
            state.requests_served.fetch_add(1, Ordering::Relaxed);
            route_request(state, &req)
        },
    );

    conn.response = response.to_bytes();
    conn.phase = ConnPhase::Writing;

    if let Err(e) = poll
        .registry()
        .reregister(&mut conn.stream, token, Interest::WRITABLE)
    {
        eprintln!("[server] Failed to reregister for writing: {e}");
        return true;
    }

    false
}

/// Handle a writable event on a client connection.
/// Returns `true` if the connection should be removed.
fn handle_write(conn: &mut ConnState) -> bool {
    loop {
        match conn.stream.write(&conn.response[conn.write_pos..]) {
            Ok(0) => {
                // Cannot make progress
                return true;
            }
            Ok(n) => {
                conn.write_pos += n;
                if conn.write_pos >= conn.response.len() {
                    // Fully written — done
                    return true;
                }
            }
            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                return false;
            }
            Err(_) => {
                return true;
            }
        }
    }
}

fn contains_header_end(buf: &[u8]) -> bool {
    buf.windows(4).any(|w| w == b"\r\n\r\n")
}

fn remove_connection(
    poll: &Poll,
    connections: &mut HashMap<Token, ConnState>,
    token: Token,
    state: &AppState,
) {
    if let Some(mut conn) = connections.remove(&token) {
        let _ = poll.registry().deregister(&mut conn.stream);
        state.active_connections.fetch_sub(1, Ordering::Relaxed);
    }
}

pub(crate) fn run(state: &Arc<AppState>) -> Result<(), AppError> {
    let addr = format!("0.0.0.0:{}", state.config.port);
    let addr: std::net::SocketAddr = addr
        .parse()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    let mut listener = TcpListener::bind(addr)?;

    let mut poll = Poll::new()?;
    let mut events = Events::with_capacity(1024);

    poll.registry()
        .register(&mut listener, SERVER, Interest::READABLE)?;

    eprintln!(
        "[server] Listening on http://localhost:{}",
        state.config.port
    );

    let mut connections: HashMap<Token, ConnState> = HashMap::new();
    let mut next_token: usize = 1;

    loop {
        if state.shutdown.load(Ordering::Relaxed) {
            eprintln!("[server] Shutdown requested");
            break;
        }

        poll.poll(&mut events, Some(POLL_TIMEOUT))?;

        for event in &events {
            match event.token() {
                SERVER => {
                    // Accept connections in a loop until WouldBlock
                    loop {
                        match listener.accept() {
                            Ok((mut stream, _addr)) => {
                                if connections.len() >= MAX_CONNECTIONS {
                                    // At capacity — stream drops and closes
                                    continue;
                                }

                                let token = Token(next_token);
                                next_token = next_token.wrapping_add(1);
                                if next_token == 0 {
                                    next_token = 1; // skip SERVER token
                                }

                                if let Err(e) = poll.registry().register(
                                    &mut stream,
                                    token,
                                    Interest::READABLE,
                                ) {
                                    eprintln!("[server] Failed to register connection: {e}");
                                    continue;
                                }

                                state.active_connections.fetch_add(1, Ordering::Relaxed);

                                connections.insert(
                                    token,
                                    ConnState {
                                        stream,
                                        phase: ConnPhase::Reading,
                                        created_at: Instant::now(),
                                        read_buf: Vec::with_capacity(1024),
                                        response: Vec::new(),
                                        write_pos: 0,
                                    },
                                );
                            }
                            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                                break;
                            }
                            Err(e) => {
                                eprintln!("[server] Accept error: {e}");
                                break;
                            }
                        }
                    }
                }
                token => {
                    // Look up the connection — if not found, skip
                    let Some(conn) = connections.get_mut(&token) else {
                        continue;
                    };

                    let should_close = match conn.phase {
                        ConnPhase::Reading => handle_read(state, conn, &poll, token),
                        ConnPhase::Writing => handle_write(conn),
                    };

                    if should_close {
                        remove_connection(&poll, &mut connections, token, state);
                    }
                }
            }
        }

        // Sweep stale connections (Slowloris defense)
        let now = Instant::now();
        let stale: Vec<Token> = connections
            .iter()
            .filter(|(_, conn)| now.duration_since(conn.created_at) > CONN_TIMEOUT)
            .map(|(token, _)| *token)
            .collect();
        for token in stale {
            remove_connection(&poll, &mut connections, token, state);
        }
    }

    Ok(())
}
