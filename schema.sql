-- AirGradientz shared schema v1
-- All implementations must produce identical tables.
-- Pragmas (WAL, busy_timeout, foreign_keys) are connection-level
-- and remain in each implementation.

CREATE TABLE IF NOT EXISTS readings (
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
CREATE INDEX IF NOT EXISTS idx_readings_device ON readings(device_id, timestamp);
