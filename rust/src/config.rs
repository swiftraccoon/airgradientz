use std::env;
use std::fs;
use std::path::Path;

use crate::json::{self, JsonValue};

#[derive(Debug)]
pub(crate) struct DeviceConfig {
    pub(crate) ip: String,
    pub(crate) label: String,
}

#[derive(Debug)]
pub(crate) struct Config {
    pub(crate) port: u16,
    pub(crate) db_path: String,
    pub(crate) devices: Vec<DeviceConfig>,
    pub(crate) poll_interval_ms: u32,
    pub(crate) fetch_timeout_ms: u32,
    pub(crate) max_api_rows: u32,
}

fn load_config_file() -> Option<JsonValue> {
    let path = env::var("CONFIG_PATH").map_or_else(
        |_| {
            if Path::new("./airgradientz.json").exists() {
                eprintln!("[config] Loaded config from ./airgradientz.json");
                Some("./airgradientz.json".to_string())
            } else if Path::new("../airgradientz.json").exists() {
                eprintln!("[config] Loaded config from ../airgradientz.json");
                Some("../airgradientz.json".to_string())
            } else {
                None
            }
        },
        |p| {
            if Path::new(&p).exists() {
                eprintln!("[config] Loaded config from CONFIG_PATH: {p}");
                Some(p)
            } else {
                eprintln!("[config] CONFIG_PATH set but unreadable: {p}");
                None
            }
        },
    );

    let path = path?;
    let content = fs::read_to_string(&path)
        .map_err(|e| eprintln!("[config] Failed to read {path}: {e}"))
        .ok()?;
    json::parse(&content)
        .map_err(|e| eprintln!("[config] JSON parse error: {e}"))
        .ok()
}

#[allow(clippy::cast_possible_truncation)]
impl Config {
    pub(crate) fn from_env() -> Self {
        // 1. Hardcoded defaults
        let mut config = Self {
            port: 3009,
            db_path: "./airgradientz.db".to_string(),
            devices: vec![
                DeviceConfig {
                    ip: "192.168.88.6".to_string(),
                    label: "outdoor".to_string(),
                },
                DeviceConfig {
                    ip: "192.168.88.159".to_string(),
                    label: "indoor".to_string(),
                },
            ],
            poll_interval_ms: 15_000,
            fetch_timeout_ms: 5_000,
            max_api_rows: 10_000,
        };

        // 2. Config file overrides
        if let Some(root) = load_config_file() {
            if let Some(JsonValue::Array(devices)) = root.get("devices") {
                let mut parsed = Vec::new();
                for dev in devices {
                    let ip = dev.get("ip").and_then(JsonValue::as_str);
                    let label = dev.get("label").and_then(JsonValue::as_str);
                    if let (Some(ip), Some(label)) = (ip, label) {
                        parsed.push(DeviceConfig {
                            ip: ip.to_string(),
                            label: label.to_string(),
                        });
                    }
                }
                if !parsed.is_empty() {
                    config.devices = parsed;
                }
            }

            if let Some(n) = root.get("pollIntervalMs").and_then(JsonValue::as_i64)
                && n > 0
            {
                config.poll_interval_ms = u32::try_from(n).unwrap_or(u32::MAX);
            }
            if let Some(n) = root.get("fetchTimeoutMs").and_then(JsonValue::as_i64)
                && n > 0
            {
                config.fetch_timeout_ms = u32::try_from(n).unwrap_or(u32::MAX);
            }
            if let Some(n) = root.get("maxApiRows").and_then(JsonValue::as_i64)
                && n > 0
            {
                config.max_api_rows = u32::try_from(n).unwrap_or(u32::MAX);
            }

            if let Some(n) = root.get("ports").and_then(|p| p.get("rust")).and_then(JsonValue::as_i64)
                && n > 0
                && n <= i64::from(u16::MAX)
            {
                config.port = u16::try_from(n).unwrap_or(config.port);
            }
        }

        // 3. Env var overrides (highest priority)
        if let Ok(v) = env::var("PORT")
            && let Ok(p) = v.parse()
        {
            config.port = p;
        }
        if let Ok(v) = env::var("DB_PATH") {
            config.db_path = v;
        }

        config
    }
}
