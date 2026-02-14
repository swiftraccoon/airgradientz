use std::time::{SystemTime, UNIX_EPOCH};

use rusqlite::{params, Connection};

use crate::error::AppError;
use crate::json::JsonValue;

pub(crate) fn initialize(conn: &Connection) -> Result<(), AppError> {
    conn.execute_batch("PRAGMA journal_mode = WAL;")?;
    conn.execute_batch("PRAGMA busy_timeout = 5000;")?;
    conn.execute_batch("PRAGMA foreign_keys = ON;")?;

    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS readings (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            timestamp INTEGER NOT NULL,
            device_id TEXT NOT NULL,
            device_type TEXT NOT NULL CHECK(device_type IN ('indoor', 'outdoor')),
            device_ip TEXT NOT NULL,
            pm01 REAL,
            pm02 REAL,
            pm10 REAL,
            pm02_compensated REAL,
            rco2 INTEGER,
            atmp REAL,
            atmp_compensated REAL,
            rhum REAL,
            rhum_compensated REAL,
            tvoc_index REAL,
            nox_index REAL,
            wifi INTEGER,
            raw_json TEXT NOT NULL
        );
        CREATE INDEX IF NOT EXISTS idx_readings_ts ON readings(timestamp);
        CREATE INDEX IF NOT EXISTS idx_readings_device ON readings(device_id, timestamp);",
    )?;

    Ok(())
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
        "INSERT INTO readings (
            timestamp, device_id, device_type, device_ip,
            pm01, pm02, pm10, pm02_compensated,
            rco2, atmp, atmp_compensated, rhum, rhum_compensated,
            tvoc_index, nox_index, wifi, raw_json
        ) VALUES (
            ?1, ?2, ?3, ?4,
            ?5, ?6, ?7, ?8,
            ?9, ?10, ?11, ?12, ?13,
            ?14, ?15, ?16, ?17
        )",
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

        json_object(vec![
            ("id", JsonValue::from_i64(self.id)),
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
        ])
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

const QUERY_COLS: &str =
    "id, timestamp, device_id, device_type, device_ip, \
     pm01, pm02, pm10, pm02_compensated, rco2, \
     atmp, atmp_compensated, rhum, rhum_compensated, \
     tvoc_index, nox_index, wifi";

pub(crate) fn query_readings(
    conn: &Connection,
    q: &ReadingQuery,
) -> Result<Vec<Reading>, AppError> {
    let want_device = q.device.as_ref().is_some_and(|d| d != "all");

    let sql = match (want_device, q.limit) {
        (false, None) => format!(
            "SELECT {QUERY_COLS} FROM readings WHERE timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC"
        ),
        (false, Some(_)) => format!(
            "SELECT {QUERY_COLS} FROM readings WHERE timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC LIMIT ?3"
        ),
        (true, None) => format!(
            "SELECT {QUERY_COLS} FROM readings WHERE device_id = ?3 AND timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC"
        ),
        (true, Some(_)) => format!(
            "SELECT {QUERY_COLS} FROM readings WHERE device_id = ?3 AND timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC LIMIT ?4"
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
    let mut stmt = conn.prepare(
        "SELECT device_id, device_type, device_ip,
                MAX(timestamp) as last_seen,
                COUNT(*) as reading_count
         FROM readings
         GROUP BY device_id
         ORDER BY device_type",
    )?;

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
    let cols_aliased = QUERY_COLS
        .split(", ")
        .map(|c| format!("r.{c}"))
        .collect::<Vec<_>>()
        .join(", ");

    let sql = format!(
        "SELECT {cols_aliased}
         FROM readings r
         INNER JOIN (
             SELECT device_id, MAX(id) as max_id
             FROM readings
             GROUP BY device_id
         ) latest ON r.id = latest.max_id"
    );

    let mut stmt = conn.prepare(&sql)?;
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

    fn indoor_fixture() -> JsonValue {
        parse(r#"{"wifi":-51,"serialno":"84fce602549c","rco2":489,"pm01":23.83,"pm02":41.67,"pm10":54.5,"pm02Compensated":31.18,"atmp":20.78,"atmpCompensated":20.78,"rhum":32.19,"rhumCompensated":32.19,"tvocIndex":423,"noxIndex":1,"model":"I-9PSL"}"#).unwrap()
    }

    fn outdoor_fixture() -> JsonValue {
        parse(r#"{"wifi":-42,"serialno":"ecda3b1d09d8","rco2":440,"pm01":23.17,"pm02":35.33,"pm10":39.17,"pm02Compensated":23.72,"atmp":9.8,"atmpCompensated":6.27,"rhum":35,"rhumCompensated":51.41,"tvocIndex":231.08,"noxIndex":1,"model":"O-1PST"}"#).unwrap()
    }

    #[test]
    fn test_insert_and_query() {
        let conn = setup_db();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: None },
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
        let data = parse(
            r#"{"wifi":-59,"serialno":"84fce602549c","rco2":null,"pm01":null,"pm02":null,"pm10":null,"atmp":null,"model":"I-9PSL"}"#,
        ).unwrap();
        insert_reading(&conn, "192.168.1.1", &data).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: None },
        ).unwrap();

        assert_eq!(readings.len(), 1);
        assert!(readings[0].rco2.is_none());
        assert!(readings[0].pm01.is_none());
        assert_eq!(readings[0].wifi, Some(-59));
    }

    #[test]
    fn test_zero_compensated_values() {
        let conn = setup_db();
        let data = parse(
            r#"{"wifi":-45,"serialno":"84fce602549c","pm02Compensated":0,"atmpCompensated":0,"rhumCompensated":0,"model":"I-9PSL"}"#,
        ).unwrap();
        insert_reading(&conn, "192.168.1.1", &data).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: None },
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
            &ReadingQuery { device: Some("84fce602549c".to_string()), from: 0, to: i64::MAX, limit: None },
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
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: Some(3) },
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
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: None },
        ).unwrap();

        assert_eq!(readings[0].device_id, "unknown");
    }

    #[test]
    fn test_reading_to_json() {
        let conn = setup_db();
        insert_reading(&conn, "192.168.1.1", &indoor_fixture()).unwrap();

        let readings = query_readings(
            &conn,
            &ReadingQuery { device: None, from: 0, to: i64::MAX, limit: None },
        ).unwrap();

        let json = readings[0].to_json();
        assert_eq!(json.get("device_id").unwrap().as_str(), Some("84fce602549c"));
        assert_eq!(json.get("device_type").unwrap().as_str(), Some("indoor"));
        assert_eq!(json.get("pm02").unwrap().as_f64(), Some(41.67));
    }
}
