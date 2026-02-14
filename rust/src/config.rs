use std::env;

#[derive(Debug)]
pub(crate) struct DeviceConfig {
    pub(crate) ip: &'static str,
    pub(crate) label: &'static str,
}

#[derive(Debug)]
pub(crate) struct Config {
    pub(crate) port: u16,
    pub(crate) db_path: String,
    pub(crate) devices: &'static [DeviceConfig],
    pub(crate) poll_interval_ms: u32,
    pub(crate) fetch_timeout_ms: u32,
    pub(crate) max_api_rows: u32,
}

static DEVICES: &[DeviceConfig] = &[
    DeviceConfig {
        ip: "192.168.88.6",
        label: "outdoor",
    },
    DeviceConfig {
        ip: "192.168.88.159",
        label: "indoor",
    },
];

impl Config {
    pub(crate) fn from_env() -> Self {
        let port = env::var("PORT")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(3009);

        let db_path = env::var("DB_PATH").unwrap_or_else(|_| "./airgradientz.db".to_string());

        Self {
            port,
            db_path,
            devices: DEVICES,
            poll_interval_ms: 15_000,
            fetch_timeout_ms: 5_000,
            max_api_rows: 10_000,
        }
    }
}
