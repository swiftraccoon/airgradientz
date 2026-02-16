use std::io::{self, BufRead, BufReader, Read, Write};
use std::net::TcpStream;
use std::time::Duration;

use crate::error::AppError;

pub(crate) fn http_get(ip: &str, path: &str, timeout: Duration) -> Result<String, AppError> {
    const MAX_RESPONSE: usize = 1024 * 1024;
    let addr = format!("{ip}:80");
    let mut stream = TcpStream::connect_timeout(
        &addr
            .parse()
            .map_err(|e| AppError::Network(format!("invalid address {addr}: {e}")))?,
        timeout,
    )?;

    stream.set_read_timeout(Some(timeout))?;
    stream.set_write_timeout(Some(timeout))?;

    write!(stream, "GET {path} HTTP/1.1\r\nHost: {ip}\r\nConnection: close\r\n\r\n")?;
    stream.flush()?;

    let mut reader = BufReader::new(stream);

    // Read status line
    let mut status_line = String::new();
    reader.read_line(&mut status_line)?;

    // Parse status code
    let status_code: u16 = status_line
        .trim()
        .split(' ')
        .nth(1)
        .and_then(|s| s.parse().ok())
        .ok_or_else(|| {
            AppError::Network(format!("invalid status line: {}", status_line.trim()))
        })?;

    if !(200..300).contains(&status_code) {
        return Err(AppError::Network(format!("HTTP {status_code}")));
    }

    // Read headers
    let mut content_length: Option<usize> = None;
    loop {
        let mut line = String::new();
        let n = reader.read_line(&mut line)?;
        if n == 0 {
            break;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() {
            break;
        }
        if let Some(colon) = trimmed.find(':') {
            let name = trimmed[..colon].trim().to_lowercase();
            let value = trimmed[colon + 1..].trim();
            if name == "content-length" {
                content_length = value.parse().ok();
            }
        }
    }

    // Read body (cap at 1 MB to prevent OOM from rogue devices)
    if let Some(len) = content_length {
        if len > MAX_RESPONSE {
            return Err(AppError::Network("response too large".into()));
        }
        let mut buf = vec![0u8; len];
        reader.read_exact(&mut buf)?;
        String::from_utf8(buf)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()).into())
    } else {
        let mut buf = String::new();
        reader.by_ref().take(MAX_RESPONSE as u64).read_to_string(&mut buf)?;
        Ok(buf)
    }
}
