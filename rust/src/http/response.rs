use crate::json::JsonValue;

#[derive(Debug)]
pub(crate) struct HttpResponse {
    status: u16,
    status_text: &'static str,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

impl HttpResponse {
    fn new(status: u16, status_text: &'static str) -> Self {
        Self {
            status,
            status_text,
            headers: vec![
                ("Connection".to_string(), "close".to_string()),
                (
                    "X-Content-Type-Options".to_string(),
                    "nosniff".to_string(),
                ),
                ("X-Frame-Options".to_string(), "DENY".to_string()),
            ],
            body: Vec::new(),
        }
    }

    pub(crate) fn ok_json(json: &JsonValue) -> Self {
        let body = json.to_string().into_bytes();
        let mut resp = Self::new(200, "OK");
        resp.headers
            .push(("Content-Type".to_string(), "application/json".to_string()));
        resp.headers
            .push(("Content-Length".to_string(), body.len().to_string()));
        resp.body = body;
        resp
    }

    pub(crate) fn ok_static(body: Vec<u8>, content_type: &str) -> Self {
        let mut resp = Self::new(200, "OK");
        resp.headers
            .push(("Content-Type".to_string(), content_type.to_string()));
        resp.headers
            .push(("Content-Length".to_string(), body.len().to_string()));
        resp.headers
            .push(("Cache-Control".to_string(), "public, max-age=600".to_string()));
        resp.body = body;
        resp
    }

    pub(crate) fn not_found() -> Self {
        let body = b"{\"error\":\"Not found\"}".to_vec();
        let mut resp = Self::new(404, "Not Found");
        resp.headers
            .push(("Content-Type".to_string(), "application/json".to_string()));
        resp.headers
            .push(("Content-Length".to_string(), body.len().to_string()));
        resp.body = body;
        resp
    }

    pub(crate) fn method_not_allowed() -> Self {
        let body = b"{\"error\":\"Method not allowed\"}".to_vec();
        let mut resp = Self::new(405, "Method Not Allowed");
        resp.headers
            .push(("Content-Type".to_string(), "application/json".to_string()));
        resp.headers
            .push(("Content-Length".to_string(), body.len().to_string()));
        resp.body = body;
        resp
    }

    pub(crate) fn internal_error(msg: &str) -> Self {
        let body = format!("{{\"error\":\"{msg}\"}}").into_bytes();
        let mut resp = Self::new(500, "Internal Server Error");
        resp.headers
            .push(("Content-Type".to_string(), "application/json".to_string()));
        resp.headers
            .push(("Content-Length".to_string(), body.len().to_string()));
        resp.body = body;
        resp
    }

    pub(crate) fn to_bytes(&self) -> Vec<u8> {
        let mut buf = Vec::with_capacity(256 + self.body.len());

        buf.extend_from_slice(
            format!("HTTP/1.1 {} {}\r\n", self.status, self.status_text).as_bytes(),
        );

        for (name, value) in &self.headers {
            buf.extend_from_slice(format!("{name}: {value}\r\n").as_bytes());
        }

        buf.extend_from_slice(b"\r\n");
        buf.extend_from_slice(&self.body);

        buf
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::json::json_object;

    #[test]
    fn test_ok_json_response() {
        let json = json_object(vec![("status", JsonValue::String("ok".into()))]);
        let resp = HttpResponse::ok_json(&json);
        assert_eq!(resp.status, 200);
        assert!(resp
            .headers
            .iter()
            .any(|(k, v)| k == "Content-Type" && v == "application/json"));
        let body_str = String::from_utf8(resp.body).unwrap();
        assert!(body_str.contains("\"status\""));
    }

    #[test]
    fn test_not_found_response() {
        let resp = HttpResponse::not_found();
        assert_eq!(resp.status, 404);
    }

    #[test]
    fn test_security_headers() {
        let resp = HttpResponse::not_found();
        assert!(resp
            .headers
            .iter()
            .any(|(k, v)| k == "X-Content-Type-Options" && v == "nosniff"));
        assert!(resp
            .headers
            .iter()
            .any(|(k, v)| k == "X-Frame-Options" && v == "DENY"));
        assert!(resp
            .headers
            .iter()
            .any(|(k, v)| k == "Connection" && v == "close"));
    }
}
