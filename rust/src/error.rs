use std::fmt;
use std::io;

use crate::json::JsonError;

#[derive(Debug)]
pub(crate) enum AppError {
    Io(io::Error),
    Db(rusqlite::Error),
    Json(JsonError),
    Network(String),
}

impl fmt::Display for AppError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(e) => write!(f, "IO error: {e}"),
            Self::Db(e) => write!(f, "DB error: {e}"),
            Self::Json(e) => write!(f, "JSON error: {e}"),
            Self::Network(msg) => write!(f, "Network error: {msg}"),
        }
    }
}

impl From<io::Error> for AppError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<rusqlite::Error> for AppError {
    fn from(e: rusqlite::Error) -> Self {
        Self::Db(e)
    }
}

impl From<JsonError> for AppError {
    fn from(e: JsonError) -> Self {
        Self::Json(e)
    }
}
