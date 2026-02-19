use std::collections::HashMap;
use std::fmt::Write as _;
use std::sync::LazyLock;
use std::time::{SystemTime, UNIX_EPOCH};

use rusqlite::{params, Connection};

use crate::error::AppError;
use crate::json::JsonValue;

const SCHEMA: &str = include_str!("../../schema.sql");
const QUERIES_RAW: &str = include_str!("../../queries.sql");

fn parse_queries_sql(content: &str) -> HashMap<&str, String> {
    let mut queries = HashMap::new();
    let mut name: Option<&str> = None;
    let mut lines: Vec<&str> = Vec::new();

    for line in content.lines() {
        if let Some(n) = line.strip_prefix("-- name: ") {
            if let Some(prev_name) = name {
                let sql = lines.join("\n");
                let sql = sql.trim().trim_end_matches(';').to_string();
                queries.insert(prev_name, sql);
            }
            name = Some(n.trim());
            lines.clear();
        } else if !line.starts_with("--") && !line.trim().is_empty() {
            lines.push(line);
        }
    }
    if let Some(prev_name) = name {
        let sql = lines.join("\n");
        let sql = sql.trim().trim_end_matches(';').to_string();
        queries.insert(prev_name, sql);
    }
    queries
}

fn convert_placeholders(sql: &str) -> String {
    let mut result = String::new();
    let mut n = 1u32;
    let mut chars = sql.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == ':'
            && chars
                .peek()
                .is_some_and(|c| c.is_ascii_lowercase() || *c == '_')
        {
            while chars
                .peek()
                .is_some_and(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || *c == '_')
            {
                chars.next();
            }
            let _ = write!(result, "?{n}");
            n += 1;
        } else {
            result.push(ch);
        }
    }
    result
}

static QUERIES: LazyLock<HashMap<&str, String>> =
    LazyLock::new(|| parse_queries_sql(QUERIES_RAW));

static INSERT_SQL: LazyLock<String> =
    LazyLock::new(|| convert_placeholders(&QUERIES["insert_reading"]));

pub(crate) fn initialize(conn: &Connection) -> Result<(), AppError> {
    conn.execute_batch("PRAGMA journal_mode = WAL;")?;
    conn.execute_batch("PRAGMA busy_timeout = 5000;")?;
    conn.execute_batch("PRAGMA foreign_keys = ON;")?;

    conn.execute_batch(SCHEMA)?;

    Ok(())
}

pub(crate) fn get_readings_count(conn: &Connection) -> Result<i64, rusqlite::Error> {
    conn.query_row(&QUERIES["count_readings"], [], |row| row.get(0))
}

pub(crate) fn now_millis() -> i64 {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    i64::try_from(millis).unwrap_or(i64::MAX)
}

fn extract_f64(data: &JsonValue, key: &str) -> Option<f64> {
    data.get(key).and_then(JsonValue::as_f64)
}

fn extract_i64(data: &JsonValue, key: &str) -> Option<i64> {
    data.get(key).and_then(JsonValue::as_i64)
}

fn extract_str<'a>(data: &'a JsonValue, key: &str) -> Option<&'a str> {
    data.get(key).and_then(JsonValue::as_str)
}

pub(crate) fn insert_reading(
    conn: &Connection,
    ip: &str,
    data: &JsonValue,
) -> Result<(), AppError> {
    let model = extract_str(data, "model").unwrap_or("");
    let device_type = if model.starts_with("I-") {
        "indoor"
    } else {
        "outdoor"
    };

    let serial = extract_str(data, "serialno").map_or_else(|| "unknown".to_string(), String::from);

    let raw_json = data.to_string();

    conn.execute(
        &INSERT_SQL,
        params![
            now_millis(),
            serial,
            device_type,
            ip,
            extract_f64(data, "pm01"),
            extract_f64(data, "pm02"),
            extract_f64(data, "pm10"),
            extract_f64(data, "pm02Compensated"),
            extract_i64(data, "rco2"),
            extract_f64(data, "atmp"),
            extract_f64(data, "atmpCompensated"),
            extract_f64(data, "rhum"),
            extract_f64(data, "rhumCompensated"),
            extract_f64(data, "tvocIndex"),
            extract_f64(data, "noxIndex"),
            extract_i64(data, "wifi"),
            raw_json,
        ],
    )?;

    Ok(())
}

pub(crate) struct ReadingQuery {
    pub(crate) device: Option<String>,
    pub(crate) from: i64,
    pub(crate) to: i64,
    pub(crate) limit: Option<u32>,
    pub(crate) downsample_ms: Option<i64>,
}

#[derive(Debug)]
pub(crate) struct Reading {
    id: i64,
    timestamp: i64,
    device_id: String,
    device_type: String,
    device_ip: String,
    pm01: Option<f64>,
    pm02: Option<f64>,
    pm10: Option<f64>,
    pm02_compensated: Option<f64>,
    rco2: Option<i64>,
    atmp: Option<f64>,
    atmp_compensated: Option<f64>,
    rhum: Option<f64>,
    rhum_compensated: Option<f64>,
    tvoc_index: Option<f64>,
    nox_index: Option<f64>,
    wifi: Option<i64>,
}

fn opt_f64_json(v: Option<f64>) -> JsonValue {
    v.map_or(JsonValue::Null, JsonValue::Number)
}

fn opt_i64_json(v: Option<i64>) -> JsonValue {
    v.map_or(JsonValue::Null, JsonValue::from_i64)
}

impl Reading {
    pub(crate) fn to_json(&self) -> JsonValue {
        use crate::json::json_object;

        let mut fields = Vec::with_capacity(17);
        // Downsampled rows have id=0 (no single row identity); omit id field.
        if self.id != 0 {
            fields.push(("id", JsonValue::from_i64(self.id)));
        }
        fields.extend_from_slice(&[
            ("timestamp", JsonValue::from_i64(self.timestamp)),
            ("device_id", JsonValue::String(self.device_id.clone())),
            ("device_type", JsonValue::String(self.device_type.clone())),
            ("device_ip", JsonValue::String(self.device_ip.clone())),
            ("pm01", opt_f64_json(self.pm01)),
            ("pm02", opt_f64_json(self.pm02)),
            ("pm10", opt_f64_json(self.pm10)),
            ("pm02_compensated", opt_f64_json(self.pm02_compensated)),
            ("rco2", opt_i64_json(self.rco2)),
            ("atmp", opt_f64_json(self.atmp)),
            ("atmp_compensated", opt_f64_json(self.atmp_compensated)),
            ("rhum", opt_f64_json(self.rhum)),
            ("rhum_compensated", opt_f64_json(self.rhum_compensated)),
            ("tvoc_index", opt_f64_json(self.tvoc_index)),
            ("nox_index", opt_f64_json(self.nox_index)),
            ("wifi", opt_i64_json(self.wifi)),
        ]);
        json_object(fields)
    }
}

fn row_to_reading(row: &rusqlite::Row<'_>) -> rusqlite::Result<Reading> {
    Ok(Reading {
        id: row.get(0)?,
        timestamp: row.get(1)?,
        device_id: row.get(2)?,
        device_type: row.get(3)?,
        device_ip: row.get(4)?,
        pm01: row.get(5)?,
        pm02: row.get(6)?,
        pm10: row.get(7)?,
        pm02_compensated: row.get(8)?,
        rco2: row.get(9)?,
        atmp: row.get(10)?,
        atmp_compensated: row.get(11)?,
        rhum: row.get(12)?,
        rhum_compensated: row.get(13)?,
        tvoc_index: row.get(14)?,
        nox_index: row.get(15)?,
        wifi: row.get(16)?,
    })
}

static QUERY_COLS: LazyLock<String> = LazyLock::new(|| {
    QUERIES["reading_columns"]
        .lines()
        .map(str::trim)
        .collect::<Vec<_>>()
        .join(" ")
});

pub(crate) fn query_readings(
    conn: &Connection,
    q: &ReadingQuery,
) -> Result<Vec<Reading>, AppError> {
    if let Some(bucket) = q.downsample_ms {
        return query_downsampled(conn, q, bucket);
    }

    let want_device = q.device.as_ref().is_some_and(|d| d != "all");

    let cols = &*QUERY_COLS;
    let sql = match (want_device, q.limit) {
        (false, None) => format!(
            "SELECT {cols} FROM readings WHERE timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC"
        ),
        (false, Some(_)) => format!(
            "SELECT {cols} FROM readings WHERE timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC LIMIT ?3"
        ),
        (true, None) => format!(
            "SELECT {cols} FROM readings WHERE device_id = ?3 AND timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC"
        ),
        (true, Some(_)) => format!(
            "SELECT {cols} FROM readings WHERE device_id = ?3 AND timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC LIMIT ?4"
        ),
    };

    let mut stmt = conn.prepare(&sql)?;

    let readings = match (want_device, q.limit) {
        (false, None) => stmt
            .query_map(params![q.from, q.to], row_to_reading)?
            .collect::<rusqlite::Result<Vec<_>>>()?,
        (false, Some(limit)) => stmt
            .query_map(params![q.from, q.to, limit], row_to_reading)?
            .collect::<rusqlite::Result<Vec<_>>>()?,
        (true, None) => stmt
            .query_map(
                params![q.from, q.to, q.device.as_ref().unwrap()],
                row_to_reading,
            )?
            .collect::<rusqlite::Result<Vec<_>>>()?,
        (true, Some(limit)) => stmt
            .query_map(
                params![q.from, q.to, q.device.as_ref().unwrap(), limit],
                row_to_reading,
            )?
            .collect::<rusqlite::Result<Vec<_>>>()?,
    };

    Ok(readings)
}

fn row_to_downsampled(row: &rusqlite::Row<'_>) -> rusqlite::Result<Reading> {
    Ok(Reading {
        id: 0,
        timestamp: row.get(0)?,
        device_id: row.get(1)?,
        device_type: row.get(2)?,
        device_ip: row.get(3)?,
        pm01: row.get(4)?,
        pm02: row.get(5)?,
        pm10: row.get(6)?,
        pm02_compensated: row.get(7)?,
        rco2: row.get(8)?,
        atmp: row.get(9)?,
        atmp_compensated: row.get(10)?,
        rhum: row.get(11)?,
        rhum_compensated: row.get(12)?,
        tvoc_index: row.get(13)?,
        nox_index: row.get(14)?,
        wifi: row.get(15)?,
    })
}

fn query_downsampled(
    conn: &Connection,
    q: &ReadingQuery,
    bucket: i64,
) -> Result<Vec<Reading>, AppError> {
    let want_device = q.device.as_ref().is_some_and(|d| d != "all");

    // bucket is a trusted constant from downsample_bucket match, safe to interpolate
    let mut sql = format!(
        "SELECT (timestamp / {bucket}) * {bucket} AS timestamp, \
         device_id, device_type, device_ip, \
         AVG(pm01) AS pm01, AVG(pm02) AS pm02, AVG(pm10) AS pm10, \
         AVG(pm02_compensated) AS pm02_compensated, \
         CAST(AVG(rco2) AS INTEGER) AS rco2, \
         AVG(atmp) AS atmp, AVG(atmp_compensated) AS atmp_compensated, \
         AVG(rhum) AS rhum, AVG(rhum_compensated) AS rhum_compensated, \
         AVG(tvoc_index) AS tvoc_index, AVG(nox_index) AS nox_index, \
         CAST(AVG(wifi) AS INTEGER) AS wifi \
         FROM readings WHERE "
    );

    let mut param_idx = 1u32;

    if want_device {
        let _ = write!(sql, "device_id = ?{param_idx} AND ");
        param_idx += 1;
    }

    let from_idx = param_idx;
    param_idx += 1;
    let to_idx = param_idx;
    param_idx += 1;

    let _ = write!(
        sql,
        "timestamp >= ?{from_idx} AND timestamp <= ?{to_idx} \
         GROUP BY (timestamp / {bucket}), device_id ORDER BY timestamp ASC"
    );

    if q.limit.is_some() {
        let _ = write!(sql, " LIMIT ?{param_idx}");
    }

    let mut stmt = conn.prepare(&sql)?;

    let readings = match (want_device, q.limit) {
        (false, None) => stmt
            .query_map(params![q.from, q.to], row_to_downsampled)?
            .collect::<rusqlite::Result<Vec<_>>>()?,
        (false, Some(limit)) => stmt
            .query_map(params![q.from, q.to, limit], row_to_downsampled)?
            .collect::<rusqlite::Result<Vec<_>>>()?,
        (true, None) => stmt
            .query_map(
                params![q.device.as_ref().unwrap(), q.from, q.to],
                row_to_downsampled,
            )?
            .collect::<rusqlite::Result<Vec<_>>>()?,
        (true, Some(limit)) => stmt
            .query_map(
                params![q.device.as_ref().unwrap(), q.from, q.to, limit],
                row_to_downsampled,
            )?
            .collect::<rusqlite::Result<Vec<_>>>()?,
    };

    Ok(readings)
}

pub(crate) fn get_filtered_count(
    conn: &Connection,
    from: i64,
    to: i64,
    device: Option<&str>,
) -> Result<i64, AppError> {
    let want_device = device.is_some_and(|d| d != "all");

    let count = if want_device {
        conn.query_row(
            "SELECT COUNT(*) FROM readings WHERE device_id = ?1 AND timestamp >= ?2 AND timestamp <= ?3",
            params![device.unwrap(), from, to],
            |row| row.get(0),
        )?
    } else {
        conn.query_row(
            "SELECT COUNT(*) FROM readings WHERE timestamp >= ?1 AND timestamp <= ?2",
            params![from, to],
            |row| row.get(0),
        )?
    };

    Ok(count)
}

#[derive(Debug)]
pub(crate) struct DeviceSummary {
    device_id: String,
    device_type: String,
    device_ip: String,
    last_seen: i64,
    reading_count: i64,
}

impl DeviceSummary {
    pub(crate) fn to_json(&self) -> JsonValue {
        use crate::json::json_object;
        json_object(vec![
            ("device_id", JsonValue::String(self.device_id.clone())),
            ("device_type", JsonValue::String(self.device_type.clone())),
            ("device_ip", JsonValue::String(self.device_ip.clone())),
            ("last_seen", JsonValue::from_i64(self.last_seen)),
            ("reading_count", JsonValue::from_i64(self.reading_count)),
        ])
    }
}

pub(crate) fn get_devices(conn: &Connection) -> Result<Vec<DeviceSummary>, AppError> {
    let mut stmt = conn.prepare(&QUERIES["select_devices"])?;

    let devices = stmt
        .query_map([], |row| {
            Ok(DeviceSummary {
                device_id: row.get(0)?,
                device_type: row.get(1)?,
                device_ip: row.get(2)?,
                last_seen: row.get(3)?,
                reading_count: row.get(4)?,
            })
        })?
        .collect::<rusqlite::Result<Vec<_>>>()?;

    Ok(devices)
}

pub(crate) fn get_latest_readings(conn: &Connection) -> Result<Vec<Reading>, AppError> {
    let mut stmt = conn.prepare(&QUERIES["select_latest"])?;
    let readings = stmt
        .query_map([], row_to_reading)?
        .collect::<rusqlite::Result<Vec<_>>>()?;

    Ok(readings)
}

pub(crate) fn checkpoint(conn: &Connection) -> Result<(), AppError> {
    conn.execute_batch("PRAGMA wal_checkpoint(TRUNCATE);")?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::json::parse;

    fn setup_db() -> Connection {
        let conn = Connection::open_in_memory().unwrap();
        initialize(&conn).unwrap();
        conn
    }

    fn load_fixtures() -> JsonValue {
        let content = std::fs::read_to_string("../test-fixtures.json")
            .expect("test-fixtures.json must be readable");
        parse(&content).expect("test-fixtures.json must be valid JSON")
    }

    fn indoor_fixture() -> JsonValue {
        let fixtures = load_fixtures();
        fixtures.get("indoorFull").unwrap().clone()
    }

    fn outdoor_fixture() -> JsonValue {
        let fixtures = load_fixtures();
        fixtures.get("outdoorFull").unwrap().clone()
    }

    #[test]
    fn test_insert_and_query() {
        let conn = setup_db();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: None, downsample_ms: None },
        ).unwrap();

        assert_eq!(readings.len(), 1);
        assert_eq!(readings[0].device_id, "84fce602549c");
        assert_eq!(readings[0].device_type, "indoor");
        assert_eq!(readings[0].device_ip, "192.168.1.1");
        assert_eq!(readings[0].pm02, Some(41.67));
        assert_eq!(readings[0].rco2, Some(489));
    }

    #[test]
    fn test_device_type_classification() {
        let conn = setup_db();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();
        insert_reading(&conn, "192.168.1.2", &outdoor_fixture()).unwrap();

        let devices = get_devices(&conn).unwrap();
        assert_eq!(devices.len(), 2);

        let indoor = devices.iter().find(|d| d.device_type == "indoor").unwrap();
        assert_eq!(indoor.device_id, "84fce602549c");

        let outdoor = devices.iter().find(|d| d.device_type == "outdoor").unwrap();
        assert_eq!(outdoor.device_id, "ecda3b1d09d8");
    }

    #[test]
    fn test_null_fields() {
        let conn = setup_db();
        let fixtures = load_fixtures();
        let data = fixtures.get("afterBoot").unwrap().clone();
        insert_reading(&conn, "192.168.1.1", &data).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: None, downsample_ms: None },
        ).unwrap();

        assert_eq!(readings.len(), 1);
        assert!(readings[0].rco2.is_none());
        assert!(readings[0].pm01.is_none());
        assert_eq!(readings[0].wifi, Some(-59));
    }

    #[test]
    fn test_zero_compensated_values() {
        let conn = setup_db();
        let fixtures = load_fixtures();
        let data = fixtures.get("zeroCompensated").unwrap().clone();
        insert_reading(&conn, "192.168.1.1", &data).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: None, downsample_ms: None },
        ).unwrap();

        assert_eq!(readings[0].pm02_compensated, Some(0.0));
        assert_eq!(readings[0].atmp_compensated, Some(0.0));
        assert_eq!(readings[0].rhum_compensated, Some(0.0));
    }

    #[test]
    fn test_query_with_device_filter() {
        let conn = setup_db();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();
        insert_reading(&conn, "192.168.1.2", &outdoor_fixture()).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: Some("84fce602549c".to_string()), from: 0, to: i64::MAX, limit: None, downsample_ms: None },
        ).unwrap();

        assert_eq!(readings.len(), 1);
        assert_eq!(readings[0].device_id, "84fce602549c");
    }

    #[test]
    fn test_query_with_limit() {
        let conn = setup_db();
        for _ in 0..5 {
            insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();
        }

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: Some(3), downsample_ms: None },
        ).unwrap();

        assert_eq!(readings.len(), 3);
    }

    #[test]
    fn test_get_latest_readings() {
        let conn = setup_db();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();
        insert_reading(&conn, "192.168.1.2", &outdoor_fixture()).unwrap();

        let latest = get_latest_readings(&conn).unwrap();
        assert_eq!(latest.len(), 2);
    }

    #[test]
    fn test_get_devices() {
        let conn = setup_db();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();
        insert_reading(&conn, "192.168.1.2", &outdoor_fixture()).unwrap();

        let devices = get_devices(&conn).unwrap();
        assert_eq!(devices.len(), 2);

        let indoor = devices.iter().find(|d| d.device_type == "indoor").unwrap();
        assert_eq!(indoor.reading_count, 2);
    }

    #[test]
    fn test_missing_serialno() {
        let conn = setup_db();
        let data = parse(r#"{"wifi":-45,"model":"I-9PSL"}"#).unwrap();
        insert_reading(&conn, "192.168.1.1", &data).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: None, downsample_ms: None },
        ).unwrap();

        assert_eq!(readings[0].device_id, "unknown");
    }

    #[test]
    fn test_parse_queries_sql() {
        let queries = parse_queries_sql(QUERIES_RAW);
        assert!(queries.contains_key("insert_reading"));
        assert!(queries.contains_key("reading_columns"));
        assert!(queries.contains_key("select_latest"));
        assert!(queries.contains_key("select_devices"));
        assert!(queries.contains_key("count_readings"));
        assert!(queries["insert_reading"].starts_with("INSERT INTO readings"));
        assert!(queries["count_readings"].starts_with("SELECT COUNT(*)"));
        // Semicolons are stripped
        assert!(!queries["count_readings"].ends_with(';'));
        assert!(!queries["insert_reading"].ends_with(';'));
    }

    #[test]
    fn test_convert_placeholders() {
        assert_eq!(
            convert_placeholders(":a, :b, :c"),
            "?1, ?2, ?3"
        );
        assert_eq!(
            convert_placeholders("WHERE device_id = :device AND timestamp >= :from"),
            "WHERE device_id = ?1 AND timestamp >= ?2"
        );
        // No placeholders
        assert_eq!(convert_placeholders("SELECT * FROM t"), "SELECT * FROM t");
        // Underscore in name
        assert_eq!(
            convert_placeholders(":tvoc_index, :nox_index"),
            "?1, ?2"
        );
    }

    #[test]
    fn test_insert_sql_converted() {
        // INSERT_SQL should have ?N placeholders, not :name
        assert!(!INSERT_SQL.contains(":timestamp"));
        assert!(INSERT_SQL.contains("?1"));
        assert!(INSERT_SQL.contains("?17"));
    }

    #[test]
    fn test_reading_to_json() {
        let conn = setup_db();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: None, downsample_ms: None },
        ).unwrap();

        let json = readings[0].to_json();
        assert_eq!(json.get("device_id").unwrap().as_str(), Some("84fce602549c"));
        assert_eq!(json.get("device_type").unwrap().as_str(), Some("indoor"));
        assert_eq!(json.get("pm02").unwrap().as_f64(), Some(41.67));
    }

    #[test]
    fn test_downsampled_query_groups_readings() {
        let conn = setup_db();

        let base_ts: i64 = 1_700_000_000_000;
        let bucket_ms: i64 = 300_000; // 5m

        // Insert 3 readings in the same 5m bucket with different pm02 values
        for (i, pm) in [10.0, 20.0, 30.0].iter().enumerate() {
            let ts = base_ts + (i as i64) * 1000; // 1s apart, same bucket
            conn.execute(
                "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, raw_json) VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
                params![ts, "dev1", "indoor", "1.1.1.1", pm, "{}"],
            ).unwrap();
        }

        // Insert 1 reading in a different bucket
        let far_ts = base_ts + bucket_ms * 2;
        conn.execute(
            "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, raw_json) VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![far_ts, "dev1", "indoor", "1.1.1.1", 100.0, "{}"],
        ).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery {
                device: None,
                from: 0,
                to: i64::MAX,
                limit: None,
                downsample_ms: Some(bucket_ms),
            },
        ).unwrap();

        // Should get 2 buckets
        assert_eq!(readings.len(), 2);

        // First bucket: average of 10, 20, 30 = 20
        assert_eq!(readings[0].pm02, Some(20.0));
        // id should be 0 for downsampled
        assert_eq!(readings[0].id, 0);

        // Second bucket: just 100
        assert_eq!(readings[1].pm02, Some(100.0));
    }

    #[test]
    fn test_downsampled_no_id_in_json() {
        let r = Reading {
            id: 0,
            timestamp: 1_700_000_000_000,
            device_id: "dev1".to_string(),
            device_type: "indoor".to_string(),
            device_ip: "1.1.1.1".to_string(),
            pm01: None, pm02: Some(15.0), pm10: None,
            pm02_compensated: None, rco2: None,
            atmp: None, atmp_compensated: None,
            rhum: None, rhum_compensated: None,
            tvoc_index: None, nox_index: None, wifi: None,
        };

        let json = r.to_json();
        assert!(json.get("id").is_none(), "downsampled reading (id=0) should not have 'id' field");
        assert_eq!(json.get("pm02").unwrap().as_f64(), Some(15.0));
    }

    #[test]
    fn test_non_downsampled_has_id_in_json() {
        let r = Reading {
            id: 5,
            timestamp: 1_700_000_000_000,
            device_id: "dev1".to_string(),
            device_type: "indoor".to_string(),
            device_ip: "1.1.1.1".to_string(),
            pm01: None, pm02: None, pm10: None,
            pm02_compensated: None, rco2: None,
            atmp: None, atmp_compensated: None,
            rhum: None, rhum_compensated: None,
            tvoc_index: None, nox_index: None, wifi: None,
        };

        let json = r.to_json();
        assert_eq!(json.get("id").unwrap().as_i64(), Some(5));
    }

    // ---- Regression tests ----

    #[test]
    fn test_readings_ordered_asc() {
        let conn = setup_db();
        let now = now_millis();

        conn.execute(
            "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, raw_json) VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![now - 3000, "dev1", "indoor", "1.1.1.1", 10.0, "{}"],
        ).unwrap();
        conn.execute(
            "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, raw_json) VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![now - 2000, "dev1", "indoor", "1.1.1.1", 20.0, "{}"],
        ).unwrap();
        conn.execute(
            "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, raw_json) VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![now - 1000, "dev1", "indoor", "1.1.1.1", 30.0, "{}"],
        ).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: None, downsample_ms: None },
        ).unwrap();

        assert_eq!(readings.len(), 3);
        assert!(readings[0].id < readings[1].id, "first id should be less than second");
        assert!(readings[1].id < readings[2].id, "second id should be less than third");
        assert!(readings[0].timestamp <= readings[1].timestamp);
        assert!(readings[1].timestamp <= readings[2].timestamp);
    }

    #[test]
    fn test_latest_by_max_id_not_timestamp() {
        let conn = setup_db();
        let now = now_millis();

        // Insert two readings for same device with SAME timestamp but different values
        conn.execute(
            "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, rco2, raw_json) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
            params![now, "dev1", "indoor", "1.1.1.1", 10.0, 400, "{}"],
        ).unwrap();
        conn.execute(
            "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, rco2, raw_json) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
            params![now, "dev1", "indoor", "1.1.1.1", 99.0, 999, "{}"],
        ).unwrap();

        let latest = get_latest_readings(&conn).unwrap();
        assert_eq!(latest.len(), 1);
        // Should be the second insert (higher id) with pm02=99
        assert_eq!(latest[0].pm02, Some(99.0));
        assert_eq!(latest[0].rco2, Some(999));
    }

    #[test]
    fn test_devices_no_first_seen() {
        let conn = setup_db();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();

        let devices = get_devices(&conn).unwrap();
        assert_eq!(devices.len(), 1);

        let json = devices[0].to_json();
        assert!(json.get("first_seen").is_none(), "devices should not contain first_seen");
        assert!(json.get("device_id").is_some());
        assert!(json.get("last_seen").is_some());
        assert!(json.get("reading_count").is_some());
    }

    #[test]
    fn test_get_filtered_count() {
        let conn = setup_db();
        let now = now_millis();

        // Insert readings with known timestamps
        conn.execute(
            "INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json) VALUES (?1, ?2, ?3, ?4, ?5)",
            params![now - 10000, "dev1", "indoor", "1.1.1.1", "{}"],
        ).unwrap();
        conn.execute(
            "INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json) VALUES (?1, ?2, ?3, ?4, ?5)",
            params![now - 5000, "dev1", "indoor", "1.1.1.1", "{}"],
        ).unwrap();
        conn.execute(
            "INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json) VALUES (?1, ?2, ?3, ?4, ?5)",
            params![now - 3000, "dev2", "outdoor", "1.1.1.2", "{}"],
        ).unwrap();

        // All devices
        let count = get_filtered_count(&conn, 0, now + 1000, Some("all")).unwrap();
        assert_eq!(count, 3);

        // Single device
        let count = get_filtered_count(&conn, 0, now + 1000, Some("dev1")).unwrap();
        assert_eq!(count, 2);

        // No device filter
        let count = get_filtered_count(&conn, 0, now + 1000, None).unwrap();
        assert_eq!(count, 3);

        // Time range filter
        let count = get_filtered_count(&conn, now - 6000, now + 1000, Some("all")).unwrap();
        assert_eq!(count, 2);
    }

    // ---- test-spec.json integration tests ----

    fn load_test_spec() -> JsonValue {
        let content = std::fs::read_to_string("../test-spec.json")
            .expect("test-spec.json must be readable");
        parse(&content).expect("test-spec.json must be valid JSON")
    }

    fn insert_multiple_fixtures(conn: &Connection, count: usize) {
        for _ in 0..count {
            insert_reading(conn, "192.168.1.1", &indoor_fixture()).unwrap();
        }
    }

    // --- Downsample bucket verification (from test-spec.json) ---

    #[test]
    fn test_spec_downsample_buckets_match_config() {
        let spec = load_test_spec();
        let config_content = std::fs::read_to_string("../airgradientz.json")
            .expect("airgradientz.json must be readable");
        let config_json = parse(&config_content).expect("airgradientz.json must be valid JSON");

        // Parse downsample buckets from config the same way Config::from_env does
        let mut config_buckets: Vec<(String, i64)> = Vec::new();
        if let JsonValue::Object(pairs) = config_json.get("downsampleBuckets").unwrap() {
            for (key, val) in pairs {
                if let Some(ms) = val.as_i64() {
                    config_buckets.push((key.clone(), ms));
                }
            }
        }

        // Iterate test-spec downsampleBuckets array and verify each
        let spec_buckets = spec.get("downsampleBuckets").unwrap().as_array().unwrap();
        for bucket in spec_buckets {
            let param = bucket.get("param").unwrap().as_str().unwrap();
            let expect_ms = bucket.get("expectMs").unwrap().as_i64().unwrap();

            let found = config_buckets.iter().find(|(k, _)| k == param);
            assert!(
                found.is_some(),
                "downsample bucket '{param}' from test-spec.json not found in config"
            );
            assert_eq!(
                found.unwrap().1, expect_ms,
                "downsample bucket '{param}': config has {} but test-spec expects {expect_ms}",
                found.unwrap().1
            );
        }

        // Also verify no extra buckets in config that aren't in spec
        for (key, _) in &config_buckets {
            let in_spec = spec_buckets.iter().any(|b| b.get("param").unwrap().as_str() == Some(key));
            assert!(in_spec, "config bucket '{key}' not found in test-spec.json");
        }
    }

    // --- Query edge cases (from test-spec.json) ---

    #[test]
    fn test_spec_from_greater_than_to_returns_empty() {
        let conn = setup_db();
        insert_multiple_fixtures(&conn, 3);

        // from > to should return empty results
        let readings = query_readings(
            &conn,
            &ReadingQuery {
                device: None,
                from: 9_999_999_999_999,
                to: 1,
                limit: None,
                downsample_ms: None,
            },
        ).unwrap();

        assert!(readings.is_empty(), "from > to should return empty results");
    }

    #[test]
    fn test_spec_nonexistent_device_returns_empty() {
        let conn = setup_db();
        insert_multiple_fixtures(&conn, 3);

        // Querying a nonexistent device should return empty
        let readings = query_readings(
            &conn,
            &ReadingQuery {
                device: Some("nonexistent-serial-xyz".to_string()),
                from: 0,
                to: i64::MAX,
                limit: None,
                downsample_ms: None,
            },
        ).unwrap();

        assert!(readings.is_empty(), "nonexistent device should return empty results");
    }

    #[test]
    fn test_spec_limit_one_returns_exactly_one() {
        let conn = setup_db();
        insert_multiple_fixtures(&conn, 5);

        let readings = query_readings(
            &conn,
            &ReadingQuery {
                device: None,
                from: 0,
                to: i64::MAX,
                limit: Some(1),
                downsample_ms: None,
            },
        ).unwrap();

        assert_eq!(readings.len(), 1, "limit=1 should return exactly 1 result");
    }

    #[test]
    fn test_spec_count_from_greater_than_to_returns_zero() {
        let conn = setup_db();
        insert_multiple_fixtures(&conn, 3);

        let count = get_filtered_count(&conn, 9_999_999_999_999, 1, None).unwrap();
        assert_eq!(count, 0, "count with from > to should return 0");
    }

    #[test]
    fn test_spec_count_nonexistent_device_returns_zero() {
        let conn = setup_db();
        insert_multiple_fixtures(&conn, 3);

        let count = get_filtered_count(
            &conn,
            0,
            i64::MAX,
            Some("nonexistent-serial-xyz"),
        ).unwrap();
        assert_eq!(count, 0, "count with nonexistent device should return 0");
    }

    // --- Response shape verification (from test-spec.json) ---

    fn assert_has_fields(json: &JsonValue, required: &[&str], context: &str) {
        for field in required {
            assert!(
                json.get(field).is_some(),
                "{context}: required field '{field}' missing"
            );
        }
    }

    fn assert_no_fields(json: &JsonValue, forbidden: &[&str], context: &str) {
        for field in forbidden {
            assert!(
                json.get(field).is_none(),
                "{context}: forbidden field '{field}' present"
            );
        }
    }

    #[test]
    fn test_spec_reading_response_shape() {
        let spec = load_test_spec();
        let shape = spec.get("responseShapes").unwrap().get("reading").unwrap();

        let required: Vec<&str> = shape
            .get("requiredFields").unwrap()
            .as_array().unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();

        let forbidden: Vec<&str> = shape
            .get("forbiddenFields").unwrap()
            .as_array().unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();

        // Create a non-downsampled reading (id != 0) and verify its JSON shape
        let conn = setup_db();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: None, downsample_ms: None },
        ).unwrap();

        let json = readings[0].to_json();
        assert_has_fields(&json, &required, "reading");
        assert_no_fields(&json, &forbidden, "reading");
    }

    #[test]
    fn test_spec_reading_downsampled_response_shape() {
        let spec = load_test_spec();
        let shape = spec.get("responseShapes").unwrap().get("readingDownsampled").unwrap();

        let required: Vec<&str> = shape
            .get("requiredFields").unwrap()
            .as_array().unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();

        let forbidden: Vec<&str> = shape
            .get("forbiddenFields").unwrap()
            .as_array().unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();

        // Create a downsampled reading (id=0) and verify its JSON shape
        let r = Reading {
            id: 0,
            timestamp: 1_700_000_000_000,
            device_id: "dev1".to_string(),
            device_type: "indoor".to_string(),
            device_ip: "1.1.1.1".to_string(),
            pm01: Some(10.0), pm02: Some(20.0), pm10: Some(30.0),
            pm02_compensated: Some(15.0), rco2: Some(400),
            atmp: Some(22.0), atmp_compensated: Some(21.5),
            rhum: Some(50.0), rhum_compensated: Some(48.0),
            tvoc_index: Some(100.0), nox_index: Some(1.0), wifi: Some(-50),
        };

        let json = r.to_json();
        assert_has_fields(&json, &required, "readingDownsampled");
        assert_no_fields(&json, &forbidden, "readingDownsampled");
    }

    #[test]
    fn test_spec_device_response_shape() {
        let spec = load_test_spec();
        let shape = spec.get("responseShapes").unwrap().get("device").unwrap();

        let required: Vec<&str> = shape
            .get("requiredFields").unwrap()
            .as_array().unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();

        let forbidden: Vec<&str> = shape
            .get("forbiddenFields").unwrap()
            .as_array().unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();

        let conn = setup_db();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();

        let devices = get_devices(&conn).unwrap();
        let json = devices[0].to_json();
        assert_has_fields(&json, &required, "device");
        assert_no_fields(&json, &forbidden, "device");
    }

    #[test]
    fn test_spec_count_response_shape() {
        let spec = load_test_spec();
        let shape = spec.get("responseShapes").unwrap().get("count").unwrap();

        let exact_fields: Vec<&str> = shape
            .get("exactFields").unwrap()
            .as_array().unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();

        let no_extra = shape.get("noExtraFields")
            .and_then(|v| if let JsonValue::Bool(b) = v { Some(*b) } else { None })
            .unwrap_or(false);

        // Build a count response the same way api.rs does
        let count_json = crate::json::json_object(vec![
            ("count", JsonValue::from_i64(42)),
        ]);

        assert_has_fields(&count_json, &exact_fields, "count");

        if no_extra {
            if let JsonValue::Object(pairs) = &count_json {
                assert_eq!(
                    pairs.len(),
                    exact_fields.len(),
                    "count response should have exactly {} fields, got {}",
                    exact_fields.len(),
                    pairs.len()
                );
            }
        }
    }

    #[test]
    fn test_spec_config_response_shape() {
        let spec = load_test_spec();
        let shape = spec.get("responseShapes").unwrap().get("config").unwrap();

        let required: Vec<&str> = shape
            .get("requiredFields").unwrap()
            .as_array().unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();

        let forbidden: Vec<&str> = shape
            .get("forbiddenFields").unwrap()
            .as_array().unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();

        // Build a config response the same way api.rs handle_config does
        let devices = vec![crate::json::json_object(vec![
            ("ip", JsonValue::String("192.168.1.1".to_string())),
            ("label", JsonValue::String("test".to_string())),
        ])];

        let buckets: Vec<(String, JsonValue)> = vec![
            ("5m".to_string(), JsonValue::from_i64(300_000)),
        ];

        let config_json = crate::json::json_object(vec![
            ("pollIntervalMs", JsonValue::Number(15000.0)),
            ("downsampleBuckets", JsonValue::Object(buckets)),
            ("devices", crate::json::json_array(devices)),
        ]);

        assert_has_fields(&config_json, &required, "config");
        assert_no_fields(&config_json, &forbidden, "config");
    }

    #[test]
    fn test_spec_error_response_shape() {
        let spec = load_test_spec();
        let shape = spec.get("responseShapes").unwrap().get("error").unwrap();

        let exact_fields: Vec<&str> = shape
            .get("exactFields").unwrap()
            .as_array().unwrap()
            .iter()
            .map(|v| v.as_str().unwrap())
            .collect();

        let no_extra = shape.get("noExtraFields")
            .and_then(|v| if let JsonValue::Bool(b) = v { Some(*b) } else { None })
            .unwrap_or(false);

        // Build an error response matching what the HTTP layer produces
        let error_json = crate::json::json_object(vec![
            ("error", JsonValue::String("test error".to_string())),
        ]);

        assert_has_fields(&error_json, &exact_fields, "error");

        if no_extra {
            if let JsonValue::Object(pairs) = &error_json {
                assert_eq!(
                    pairs.len(),
                    exact_fields.len(),
                    "error response should have exactly {} fields, got {}",
                    exact_fields.len(),
                    pairs.len()
                );
            }
        }
    }
}
