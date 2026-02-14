use std::io::{self, BufRead, BufReader};
use std::net::TcpStream;
use std::time::Duration;

const MAX_HEADER_BYTES: usize = 8192;
const READ_TIMEOUT: Duration = Duration::from_secs(10);

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Method {
    Get,
    Other(String),
}

#[derive(Debug)]
pub(crate) struct HttpRequest {
    pub(crate) method: Method,
    pub(crate) path: String,
    query: String,
}

impl HttpRequest {
    pub(crate) fn parse(stream: &TcpStream) -> Result<Self, io::Error> {
        stream.set_read_timeout(Some(READ_TIMEOUT))?;

        let mut reader = BufReader::new(stream);
        let mut total_read = 0;

        // Read request line
        let mut request_line = String::new();
        let n = reader.read_line(&mut request_line)?;
        if n == 0 {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "empty request"));
        }
        total_read += n;

        let trimmed = request_line.trim_end();
        let mut parts = trimmed.splitn(3, ' ');

        let method_str = parts
            .next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "missing method"))?;
        let raw_path = parts
            .next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "missing path"))?;

        let method = if method_str == "GET" {
            Method::Get
        } else {
            Method::Other(method_str.to_string())
        };

        // Split path and query string
        let (path, query) = raw_path.find('?').map_or_else(
            || (raw_path.to_string(), String::new()),
            |idx| {
                (
                    raw_path[..idx].to_string(),
                    raw_path[idx + 1..].to_string(),
                )
            },
        );

        // Read headers (consume but don't store)
        loop {
            let mut line = String::new();
            let n = reader.read_line(&mut line)?;
            if n == 0 {
                break;
            }
            total_read += n;
            if total_read > MAX_HEADER_BYTES {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "headers too large",
                ));
            }

            let header_line = line.trim_end_matches(['\r', '\n']);
            if header_line.is_empty() {
                break;
            }
        }

        Ok(Self {
            method,
            path,
            query,
        })
    }

    pub(crate) fn query_param(&self, name: &str) -> Option<String> {
        if self.query.is_empty() {
            return None;
        }
        for pair in self.query.split('&') {
            if let Some(eq_pos) = pair.find('=') {
                if &pair[..eq_pos] == name {
                    return Some(url_decode(&pair[eq_pos + 1..]));
                }
            } else if pair == name {
                return Some(String::new());
            }
        }
        None
    }
}

fn url_decode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%'
            && i + 2 < bytes.len()
            && let Ok(hex_str) = std::str::from_utf8(&bytes[i + 1..i + 3])
            && let Ok(byte) = u8::from_str_radix(hex_str, 16)
        {
            result.push(byte as char);
            i += 3;
            continue;
        }
        if bytes[i] == b'+' {
            result.push(' ');
        } else {
            result.push(bytes[i] as char);
        }
        i += 1;
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::net::TcpListener;

    fn parse_request(raw: &str) -> HttpRequest {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let addr = listener.local_addr().unwrap();
        let raw = raw.to_string();
        let handle = std::thread::spawn(move || {
            let mut client = TcpStream::connect(addr).unwrap();
            client.write_all(raw.as_bytes()).unwrap();
            client.flush().unwrap();
        });
        let (stream, _) = listener.accept().unwrap();
        let req = HttpRequest::parse(&stream).unwrap();
        handle.join().unwrap();
        req
    }

    #[test]
    fn test_parse_get() {
        let req = parse_request("GET /api/readings HTTP/1.1\r\nHost: localhost\r\n\r\n");
        assert_eq!(req.method, Method::Get);
        assert_eq!(req.path, "/api/readings");
        assert!(req.query.is_empty());
    }

    #[test]
    fn test_parse_query_string() {
        let req = parse_request("GET /api/readings?from=100&to=200 HTTP/1.1\r\n\r\n");
        assert_eq!(req.path, "/api/readings");
        assert_eq!(req.query_param("from"), Some("100".to_string()));
        assert_eq!(req.query_param("to"), Some("200".to_string()));
        assert_eq!(req.query_param("missing"), None);
    }

    #[test]
    fn test_parse_non_get() {
        let req = parse_request("POST /api/readings HTTP/1.1\r\n\r\n");
        assert_eq!(req.method, Method::Other("POST".to_string()));
    }
}
