use std::sync::atomic::Ordering;

use crate::db::{self, ReadingQuery};
use crate::http::request::HttpRequest;
use crate::http::response::HttpResponse;
use crate::json::{json_array, json_object, JsonValue};
use crate::log::log;
use crate::poller;
use crate::AppState;

pub(crate) fn handle_readings(state: &AppState, req: &HttpRequest) -> HttpResponse {
    let Ok(conn) = state.db.lock() else {
        return HttpResponse::internal_error("Internal server error");
    };

    let now = db::now_millis();
    let default_from = now - 24 * 60 * 60 * 1000;

    let from: i64 = req
        .query_param("from")
        .and_then(|s| s.parse().ok())
        .unwrap_or(default_from);

    let to: i64 = req
        .query_param("to")
        .and_then(|s| s.parse().ok())
        .unwrap_or(now);

    let device = req.query_param("device");

    let requested_limit: u32 = req
        .query_param("limit")
        .and_then(|s| s.parse().ok())
        .unwrap_or(state.config.max_api_rows);

    let effective_limit = if requested_limit > 0 {
        requested_limit.min(state.config.max_api_rows)
    } else {
        state.config.max_api_rows
    };

    // Parse downsample parameter
    let downsample_ms = if let Some(ds) = req.query_param("downsample") {
        match state.config.downsample_buckets.iter().find(|(k, _)| k == &ds) {
            Some((_, ms)) => Some(*ms),
            None => return HttpResponse::bad_request("invalid downsample value"),
        }
    } else {
        None
    };

    let query = ReadingQuery {
        device: Some(device.unwrap_or_else(|| "all".to_string())),
        from,
        to,
        limit: Some(effective_limit),
        downsample_ms,
    };

    match db::query_readings(&conn, &query) {
        Ok(readings) => {
            // Pre-allocate: ~300 bytes per reading for typical JSON output
            let mut buf = String::with_capacity(readings.len() * 300 + 2);
            buf.push('[');
            for (i, reading) in readings.iter().enumerate() {
                if i > 0 {
                    buf.push(',');
                }
                reading.write_json(&mut buf);
            }
            buf.push(']');
            HttpResponse::ok_json_raw(buf)
        }
        Err(e) => {
            log!("[api] query_readings error: {e}");
            HttpResponse::internal_error("Internal server error")
        }
    }
}

pub(crate) fn handle_readings_count(state: &AppState, req: &HttpRequest) -> HttpResponse {
    let Ok(conn) = state.db.lock() else {
        return HttpResponse::internal_error("Internal server error");
    };

    let now = db::now_millis();

    let from: i64 = req
        .query_param("from")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);

    let to: i64 = req
        .query_param("to")
        .and_then(|s| s.parse().ok())
        .unwrap_or(now);

    let device = req.query_param("device");

    match db::get_filtered_count(&conn, from, to, device.as_deref()) {
        Ok(count) => {
            let body = json_object(vec![("count", JsonValue::from_i64(count))]);
            let serialized = crate::json::serialize_to_string(&body, 32);
            HttpResponse::ok_json_raw(serialized)
        }
        Err(e) => {
            log!("[api] get_filtered_count error: {e}");
            HttpResponse::internal_error("Internal server error")
        }
    }
}

pub(crate) fn handle_readings_latest(state: &AppState) -> HttpResponse {
    let Ok(conn) = state.db.lock() else {
        return HttpResponse::internal_error("Internal server error");
    };

    match db::get_latest_readings(&conn) {
        Ok(readings) => {
            let mut buf = String::with_capacity(readings.len() * 300 + 2);
            buf.push('[');
            for (i, reading) in readings.iter().enumerate() {
                if i > 0 {
                    buf.push(',');
                }
                reading.write_json(&mut buf);
            }
            buf.push(']');
            HttpResponse::ok_json_raw(buf)
        }
        Err(e) => {
            log!("[api] get_latest_readings error: {e}");
            HttpResponse::internal_error("Internal server error")
        }
    }
}

pub(crate) fn handle_devices(state: &AppState) -> HttpResponse {
    let Ok(conn) = state.db.lock() else {
        return HttpResponse::internal_error("Internal server error");
    };

    match db::get_devices(&conn) {
        Ok(devices) => {
            let mut buf = String::with_capacity(devices.len() * 150 + 2);
            buf.push('[');
            for (i, device) in devices.iter().enumerate() {
                if i > 0 {
                    buf.push(',');
                }
                device.write_json(&mut buf);
            }
            buf.push(']');
            HttpResponse::ok_json_raw(buf)
        }
        Err(e) => {
            log!("[api] get_devices error: {e}");
            HttpResponse::internal_error("Internal server error")
        }
    }
}

pub(crate) fn handle_health(state: &AppState) -> HttpResponse {
    let json = poller::get_health_json(state);
    let body = crate::json::serialize_to_string(&json, 256);
    HttpResponse::ok_json_raw(body)
}

pub(crate) fn handle_config(state: &AppState) -> HttpResponse {
    let devices: Vec<JsonValue> = state
        .config
        .devices
        .iter()
        .map(|d| {
            json_object(vec![
                ("ip", JsonValue::String(d.ip.clone())),
                ("label", JsonValue::String(d.label.clone())),
            ])
        })
        .collect();

    let buckets: Vec<(std::borrow::Cow<'static, str>, JsonValue)> = state
        .config
        .downsample_buckets
        .iter()
        .map(|(key, ms)| (std::borrow::Cow::Owned(key.clone()), JsonValue::from_i64(*ms)))
        .collect();

    let config = json_object(vec![
        (
            "pollIntervalMs",
            JsonValue::Number(f64::from(state.config.poll_interval_ms)),
        ),
        (
            "downsampleBuckets",
            JsonValue::Object(buckets),
        ),
        ("devices", json_array(devices)),
    ]);

    let body = crate::json::serialize_to_string(&config, 512);
    HttpResponse::ok_json_raw(body)
}

fn read_rss_bytes() -> u64 {
    std::fs::read_to_string("/proc/self/statm").map_or(0, |contents| {
        contents
            .split_whitespace()
            .nth(1)
            .and_then(|s| s.parse::<u64>().ok())
            .map_or(0, |pages| pages * 4096)
    })
}

#[allow(clippy::cast_precision_loss)]
pub(crate) fn handle_stats(state: &AppState) -> HttpResponse {
    let now = db::now_millis();
    let uptime_ms = now - state.started_at;

    let readings_count = state
        .db
        .lock()
        .ok()
        .and_then(|conn| db::get_readings_count(&conn).ok())
        .unwrap_or(0);

    let db_size_bytes = std::fs::metadata(&state.config.db_path)
        .map(|m| m.len())
        .unwrap_or(0);

    let requests_served = state.requests_served.load(Ordering::Relaxed);
    let active_connections = state.active_connections.load(Ordering::Relaxed);
    let (poll_successes, poll_failures) = poller::get_poll_stats();

    let body = json_object(vec![
        ("implementation", JsonValue::String("rust".to_string())),
        ("pid", JsonValue::Number(f64::from(std::process::id()))),
        ("uptime_ms", JsonValue::from_i64(uptime_ms)),
        ("memory_rss_bytes", JsonValue::Number(read_rss_bytes() as f64)),
        ("db_size_bytes", JsonValue::Number(db_size_bytes as f64)),
        ("readings_count", JsonValue::from_i64(readings_count)),
        ("requests_served", JsonValue::Number(requests_served as f64)),
        ("active_connections", JsonValue::from_i64(active_connections)),
        ("poll_successes", JsonValue::Number(poll_successes as f64)),
        ("poll_failures", JsonValue::Number(poll_failures as f64)),
        ("pool_alloc_count", JsonValue::Number(0.0)),
        ("pool_bytes_used", JsonValue::Number(0.0)),
        ("started_at", JsonValue::from_i64(state.started_at)),
    ]);

    let serialized = crate::json::serialize_to_string(&body, 512);
    HttpResponse::ok_json_raw(serialized)
}
