use std::env;
use std::fs;
use std::path::Path;

use crate::json::{self, JsonValue};
use crate::log::log;

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
    pub(crate) downsample_buckets: Vec<(String, i64)>,
}

fn find_config_path() -> Option<String> {
    if let Ok(p) = env::var("CONFIG_PATH") {
        if Path::new(&p).exists() {
            log!("[config] Loaded config from CONFIG_PATH: {p}");
            return Some(p);
        }
        log!("[config] CONFIG_PATH set but unreadable: {p}");
        return None;
    }
    if Path::new("./airgradientz.json").exists() {
        log!("[config] Loaded config from ./airgradientz.json");
        return Some("./airgradientz.json".to_string());
    }
    if Path::new("../airgradientz.json").exists() {
        log!("[config] Loaded config from ../airgradientz.json");
        return Some("../airgradientz.json".to_string());
    }
    None
}

#[allow(clippy::cast_possible_truncation)]
impl Config {
    pub(crate) fn from_env() -> Self {
        // 1. Config file is mandatory
        let path = find_config_path().unwrap_or_else(|| {
            eprintln!("fatal: config file not found");
            std::process::exit(1);
        });
        let content = fs::read_to_string(&path).unwrap_or_else(|e| {
            eprintln!("fatal: config file not found");
            eprintln!("[config] Failed to read {path}: {e}");
            std::process::exit(1);
        });
        let root = json::parse(&content).unwrap_or_else(|e| {
            eprintln!("fatal: config file not found");
            eprintln!("[config] JSON parse error: {e}");
            std::process::exit(1);
        });

        // 2. Validate all required keys, collecting missing ones
        let mut missing: Vec<&str> = Vec::new();

        let poll_interval_ms = root
            .get("pollIntervalMs")
            .and_then(JsonValue::as_i64)
            .filter(|&n| n > 0);
        if poll_interval_ms.is_none() {
            missing.push("pollIntervalMs");
        }

        let fetch_timeout_ms = root
            .get("fetchTimeoutMs")
            .and_then(JsonValue::as_i64)
            .filter(|&n| n > 0);
        if fetch_timeout_ms.is_none() {
            missing.push("fetchTimeoutMs");
        }

        let max_api_rows = root
            .get("maxApiRows")
            .and_then(JsonValue::as_i64)
            .filter(|&n| n > 0);
        if max_api_rows.is_none() {
            missing.push("maxApiRows");
        }

        let mut downsample_buckets = Vec::new();
        if let Some(JsonValue::Object(pairs)) = root.get("downsampleBuckets") {
            for (key, val) in pairs {
                if let Some(ms) = val.as_i64()
                    && ms > 0
                {
                    downsample_buckets.push((key.clone(), ms));
                }
            }
        }
        if downsample_buckets.is_empty() {
            missing.push("downsampleBuckets");
        }

        // Parse devices
        let mut devices = Vec::new();
        if let Some(JsonValue::Array(arr)) = root.get("devices") {
            for dev in arr {
                let ip = dev.get("ip").and_then(JsonValue::as_str);
                let label = dev.get("label").and_then(JsonValue::as_str);
                if let (Some(ip), Some(label)) = (ip, label) {
                    devices.push(DeviceConfig {
                        ip: ip.to_string(),
                        label: label.to_string(),
                    });
                }
            }
        }
        if devices.is_empty() {
            missing.push("devices");
        }

        // Parse port from ports.rust
        let port_val = root
            .get("ports")
            .and_then(|p| p.get("rust"))
            .and_then(JsonValue::as_i64)
            .filter(|&n| n > 0 && n <= i64::from(u16::MAX));
        if port_val.is_none() {
            missing.push("ports.rust");
        }

        if !missing.is_empty() {
            eprintln!("fatal: missing required config keys: {}", missing.join(", "));
            std::process::exit(1);
        }

        // 3. Construct config from validated values
        let mut config = Self {
            port: u16::try_from(port_val.unwrap()).unwrap(),
            db_path: "./airgradientz.db".to_string(),
            devices,
            poll_interval_ms: u32::try_from(poll_interval_ms.unwrap()).unwrap_or(u32::MAX),
            fetch_timeout_ms: u32::try_from(fetch_timeout_ms.unwrap()).unwrap_or(u32::MAX),
            max_api_rows: u32::try_from(max_api_rows.unwrap()).unwrap_or(u32::MAX),
            downsample_buckets,
        };

        // 4. Env var overrides (highest priority)
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
