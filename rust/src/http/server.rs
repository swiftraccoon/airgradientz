use std::fs;
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::thread;

use crate::api;
use crate::error::AppError;
use crate::http::request::{HttpRequest, Method};
use crate::http::response::HttpResponse;
use crate::AppState;

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
    if req_path.contains("..") {
        return HttpResponse::not_found();
    }

    let relative = req_path.trim_start_matches('/');
    let file_path = if relative.is_empty() {
        PathBuf::from("public/index.html")
    } else {
        PathBuf::from("public").join(relative)
    };

    fs::read(&file_path).map_or_else(
        |_| HttpResponse::not_found(),
        |contents| {
            let ct = content_type_for(&file_path);
            HttpResponse::ok_static(contents, ct)
        },
    )
}

fn handle_connection(state: &AppState, stream: std::net::TcpStream) {
    state.active_connections.fetch_add(1, Ordering::Relaxed);

    let result = handle_request(state, stream);

    state.active_connections.fetch_sub(1, Ordering::Relaxed);

    if let Err(msg) = result {
        eprintln!("[server] {msg}");
    }
}

fn handle_request(
    state: &AppState,
    stream: std::net::TcpStream,
) -> Result<(), String> {
    let req = HttpRequest::parse(&stream)
        .map_err(|e| format!("Failed to parse request: {e}"))?;

    state.requests_served.fetch_add(1, Ordering::Relaxed);

    let response = if req.method == Method::Get {
        match req.path.as_str() {
            "/api/readings" => api::handle_readings(state, &req),
            "/api/readings/latest" => api::handle_readings_latest(state),
            "/api/devices" => api::handle_devices(state),
            "/api/health" => api::handle_health(state),
            "/api/config" => api::handle_config(state),
            "/api/stats" => api::handle_stats(state),
            path => serve_static(path),
        }
    } else {
        HttpResponse::method_not_allowed()
    };

    let mut stream = stream;
    response
        .write_to(&mut stream)
        .map_err(|e| format!("Failed to write response: {e}"))
}

pub(crate) fn run(state: &Arc<AppState>) -> Result<(), AppError> {
    let addr = format!("0.0.0.0:{}", state.config.port);
    let listener = TcpListener::bind(&addr)?;

    eprintln!(
        "[server] Listening on http://localhost:{}",
        state.config.port
    );

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                let state = Arc::clone(state);
                thread::spawn(move || {
                    handle_connection(&state, stream);
                });
            }
            Err(e) => {
                eprintln!("[server] Connection error: {e}");
            }
        }
    }

    Ok(())
}
