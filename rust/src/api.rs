use crate::db::{self, ReadingQuery};
use crate::http::request::HttpRequest;
use crate::http::response::HttpResponse;
use crate::json::{json_array, json_object, JsonValue};
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

    let query = ReadingQuery {
        device: Some(device.unwrap_or_else(|| "all".to_string())),
        from,
        to,
        limit: Some(effective_limit),
    };

    match db::query_readings(&conn, &query) {
        Ok(readings) => {
            let items: Vec<JsonValue> = readings.iter().map(db::Reading::to_json).collect();
            HttpResponse::ok_json(&json_array(items))
        }
        Err(e) => {
            eprintln!("[api] query_readings error: {e}");
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
            let items: Vec<JsonValue> = readings.iter().map(db::Reading::to_json).collect();
            HttpResponse::ok_json(&json_array(items))
        }
        Err(e) => {
            eprintln!("[api] get_latest_readings error: {e}");
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
            let items: Vec<JsonValue> = devices.iter().map(db::DeviceSummary::to_json).collect();
            HttpResponse::ok_json(&json_array(items))
        }
        Err(e) => {
            eprintln!("[api] get_devices error: {e}");
            HttpResponse::internal_error("Internal server error")
        }
    }
}

pub(crate) fn handle_health(state: &AppState) -> HttpResponse {
    let json = poller::get_health_json(state);
    HttpResponse::ok_json(&json)
}

pub(crate) fn handle_config(state: &AppState) -> HttpResponse {
    let devices: Vec<JsonValue> = state
        .config
        .devices
        .iter()
        .map(|d| {
            json_object(vec![
                ("ip", JsonValue::String(d.ip.to_string())),
                ("label", JsonValue::String(d.label.to_string())),
            ])
        })
        .collect();

    let config = json_object(vec![
        (
            "pollIntervalMs",
            JsonValue::Number(f64::from(state.config.poll_interval_ms)),
        ),
        ("devices", json_array(devices)),
    ]);

    HttpResponse::ok_json(&config)
}
