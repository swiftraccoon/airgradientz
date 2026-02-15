use std::io;

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
    pub(crate) fn parse_from_buf(buf: &[u8]) -> Result<Self, io::Error> {
        let data = std::str::from_utf8(buf)
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "invalid UTF-8"))?;

        let line_end = data
            .find("\r\n")
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "incomplete request"))?;

        let request_line = &data[..line_end];
        let mut parts = request_line.splitn(3, ' ');

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

        let (path, query) = raw_path.find('?').map_or_else(
            || (raw_path.to_string(), String::new()),
            |idx| {
                (
                    raw_path[..idx].to_string(),
                    raw_path[idx + 1..].to_string(),
                )
            },
        );

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

pub(crate) fn url_decode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%'
            && i + 2 < bytes.len()
            && let Ok(hex_str) = std::str::from_utf8(&bytes[i + 1..i + 3])
            && let Ok(byte) = u8::from_str_radix(hex_str, 16)
        {
            if byte == 0 {
                // Reject null bytes
                i += 3;
                continue;
            }
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

    #[test]
    fn test_parse_get() {
        let req =
            HttpRequest::parse_from_buf(b"GET /api/readings HTTP/1.1\r\nHost: localhost\r\n\r\n")
                .unwrap();
        assert_eq!(req.method, Method::Get);
        assert_eq!(req.path, "/api/readings");
        assert!(req.query.is_empty());
    }

    #[test]
    fn test_parse_query_string() {
        let req =
            HttpRequest::parse_from_buf(b"GET /api/readings?from=100&to=200 HTTP/1.1\r\n\r\n")
                .unwrap();
        assert_eq!(req.path, "/api/readings");
        assert_eq!(req.query_param("from"), Some("100".to_string()));
        assert_eq!(req.query_param("to"), Some("200".to_string()));
        assert_eq!(req.query_param("missing"), None);
    }

    #[test]
    fn test_parse_non_get() {
        let req =
            HttpRequest::parse_from_buf(b"POST /api/readings HTTP/1.1\r\n\r\n").unwrap();
        assert_eq!(req.method, Method::Other("POST".to_string()));
    }
}
