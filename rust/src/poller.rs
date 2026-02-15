use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

use crate::db;
use crate::http::client::http_get;
use crate::json;
use crate::json::{json_array, json_object, JsonValue};
use crate::AppState;

const CHECKPOINT_INTERVAL_POLLS: u32 = 10; // ~5min at 30s intervals

static POLL_SUCCESSES: AtomicU64 = AtomicU64::new(0);
static POLL_FAILURES: AtomicU64 = AtomicU64::new(0);

pub(crate) fn get_poll_stats() -> (u64, u64) {
    (
        POLL_SUCCESSES.load(Ordering::Relaxed),
        POLL_FAILURES.load(Ordering::Relaxed),
    )
}

#[derive(Debug, Clone)]
pub(crate) enum HealthStatus {
    Unknown,
    Ok,
    Error,
}

#[derive(Debug, Clone)]
pub(crate) struct DeviceHealth {
    ip: String,
    label: String,
    status: HealthStatus,
    last_success: Option<i64>,
    last_error: Option<i64>,
    last_error_message: Option<String>,
    consecutive_failures: u32,
}

fn opt_ts_to_json(ts: Option<i64>) -> JsonValue {
    ts.map_or(JsonValue::Null, JsonValue::from_i64)
}

impl DeviceHealth {
    fn to_json(&self) -> JsonValue {
        json_object(vec![
            ("ip", JsonValue::String(self.ip.clone())),
            ("label", JsonValue::String(self.label.clone())),
            (
                "status",
                JsonValue::String(match self.status {
                    HealthStatus::Unknown => "unknown".to_string(),
                    HealthStatus::Ok => "ok".to_string(),
                    HealthStatus::Error => "error".to_string(),
                }),
            ),
            ("lastSuccess", opt_ts_to_json(self.last_success)),
            ("lastError", opt_ts_to_json(self.last_error)),
            (
                "lastErrorMessage",
                self.last_error_message
                    .as_ref()
                    .map_or(JsonValue::Null, |msg| JsonValue::String(msg.clone())),
            ),
            (
                "consecutiveFailures",
                JsonValue::Number(f64::from(self.consecutive_failures)),
            ),
        ])
    }
}

fn init_health(state: &AppState) {
    let mut health = state.health.write().unwrap_or_else(|e| e.into_inner());
    health.clear();
    for device in &state.config.devices {
        health.insert(
            device.ip.clone(),
            DeviceHealth {
                ip: device.ip.clone(),
                label: device.label.clone(),
                status: HealthStatus::Unknown,
                last_success: None,
                last_error: None,
                last_error_message: None,
                consecutive_failures: 0,
            },
        );
    }
}

pub(crate) fn get_health_json(state: &AppState) -> JsonValue {
    let health = state.health.read().unwrap_or_else(|e| e.into_inner());
    let items: Vec<JsonValue> = state
        .config
        .devices
        .iter()
        .filter_map(|d| health.get(&d.ip).map(DeviceHealth::to_json))
        .collect();
    json_array(items)
}

fn record_success(health: &mut HashMap<String, DeviceHealth>, ip: &str) {
    if let Some(h) = health.get_mut(ip) {
        h.status = HealthStatus::Ok;
        h.last_success = Some(crate::db::now_millis());
        h.last_error_message = None;
        h.consecutive_failures = 0;
    }
}

fn record_failure(health: &mut HashMap<String, DeviceHealth>, ip: &str, message: String) {
    if let Some(h) = health.get_mut(ip) {
        h.status = HealthStatus::Error;
        h.last_error = Some(crate::db::now_millis());
        h.last_error_message = Some(message);
        h.consecutive_failures += 1;
    }
}

fn format_optional<T: std::fmt::Display>(val: Option<T>) -> String {
    val.map_or_else(|| "N/A".to_string(), |v| v.to_string())
}

fn fetch_device(state: &AppState, ip: &str, label: &str) {
    let timeout = Duration::from_millis(u64::from(state.config.fetch_timeout_ms));

    match http_get(ip, "/measures/current", timeout) {
        Err(e) => {
            POLL_FAILURES.fetch_add(1, Ordering::Relaxed);
            let msg = e.to_string();
            eprintln!("[poller] {label} ({ip}): fetch failed: {msg}");
            let mut health = state.health.write().unwrap_or_else(|e| e.into_inner());
            record_failure(&mut health, ip, msg);
        }
        Ok(body) => match json::parse(&body) {
            Err(e) => {
                POLL_FAILURES.fetch_add(1, Ordering::Relaxed);
                let msg = format!("JSON parse error: {e}");
                eprintln!("[poller] {label} ({ip}): {msg}");
                let mut health = state.health.write().unwrap_or_else(|e| e.into_inner());
                record_failure(&mut health, ip, msg);
            }
            Ok(data) => {
                if !data.is_object() {
                    POLL_FAILURES.fetch_add(1, Ordering::Relaxed);
                    let msg = "unexpected response type: not an object".to_string();
                    eprintln!("[poller] {label} ({ip}): {msg}");
                    let mut health = state.health.write().unwrap_or_else(|e| e.into_inner());
                    record_failure(&mut health, ip, msg);
                    return;
                }

                let db_result = {
                    let conn = state.db.lock().unwrap_or_else(|e| e.into_inner());
                    db::insert_reading(&conn, ip, &data)
                };

                if let Err(e) = db_result {
                    POLL_FAILURES.fetch_add(1, Ordering::Relaxed);
                    let msg = format!("DB insert failed: {e}");
                    eprintln!("[poller] {label} ({ip}): {msg}");
                    let mut health = state.health.write().unwrap_or_else(|e| e.into_inner());
                    record_failure(&mut health, ip, msg);
                } else {
                    POLL_SUCCESSES.fetch_add(1, Ordering::Relaxed);
                    let pm02 = data.get("pm02").and_then(JsonValue::as_f64);
                    let rco2 = data.get("rco2").and_then(JsonValue::as_i64);
                    let atmp = data.get("atmp").and_then(JsonValue::as_f64);

                    let mut health = state.health.write().unwrap_or_else(|e| e.into_inner());
                    record_success(&mut health, ip);
                    eprintln!(
                        "[poller] {label} ({ip}): OK — PM2.5={}, CO2={}, T={}°C",
                        format_optional(pm02),
                        format_optional(rco2),
                        format_optional(atmp),
                    );
                }
            }
        },
    }
}

fn poll_all(state: &AppState) {
    for device in &state.config.devices {
        fetch_device(state, &device.ip, &device.label);
    }
}

pub(crate) fn run(state: &Arc<AppState>) {
    let poll_interval = Duration::from_millis(u64::from(state.config.poll_interval_ms));

    eprintln!(
        "[poller] Starting — polling {} devices every {}s",
        state.config.devices.len(),
        state.config.poll_interval_ms / 1000,
    );

    init_health(state);
    poll_all(state);

    let mut poll_count: u32 = 0;

    loop {
        thread::sleep(poll_interval);

        if state.shutdown.load(std::sync::atomic::Ordering::Relaxed) {
            eprintln!("[poller] Stopped");
            break;
        }

        poll_all(state);
        poll_count += 1;

        if poll_count.is_multiple_of(CHECKPOINT_INTERVAL_POLLS) {
            let conn = state.db.lock().unwrap_or_else(|e| e.into_inner());
            if let Err(e) = db::checkpoint(&conn) {
                eprintln!("[poller] WAL checkpoint failed: {e}");
            }
        }
    }
}
