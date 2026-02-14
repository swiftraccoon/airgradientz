'use strict';

const Database = require('better-sqlite3');
const config = require('../config');

const db = new Database(config.dbPath);
db.pragma('journal_mode = WAL');
db.pragma('busy_timeout = 5000');
db.pragma('foreign_keys = ON');

db.exec(`
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
`);

// Columns returned by query endpoints (excludes raw_json to keep responses small)
const QUERY_COLS = [
  'id', 'timestamp', 'device_id', 'device_type', 'device_ip',
  'pm01', 'pm02', 'pm10', 'pm02_compensated', 'rco2',
  'atmp', 'atmp_compensated', 'rhum', 'rhum_compensated',
  'tvoc_index', 'nox_index', 'wifi',
].join(', ');

const insertStmt = db.prepare(`
  INSERT INTO readings (
    timestamp, device_id, device_type, device_ip,
    pm01, pm02, pm10, pm02_compensated,
    rco2, atmp, atmp_compensated, rhum, rhum_compensated,
    tvoc_index, nox_index, wifi, raw_json
  ) VALUES (
    @timestamp, @device_id, @device_type, @device_ip,
    @pm01, @pm02, @pm10, @pm02_compensated,
    @rco2, @atmp, @atmp_compensated, @rhum, @rhum_compensated,
    @tvoc_index, @nox_index, @wifi, @raw_json
  )
`);

// Pre-prepared statements for all query variants (avoids per-request prepare leak)
const queryStmts = Object.freeze({
  allDevRange:      db.prepare(`SELECT ${QUERY_COLS} FROM readings WHERE timestamp >= @from AND timestamp <= @to ORDER BY timestamp ASC`),
  allDevRangeLimit: db.prepare(`SELECT ${QUERY_COLS} FROM readings WHERE timestamp >= @from AND timestamp <= @to ORDER BY timestamp ASC LIMIT @limit`),
  oneDevRange:      db.prepare(`SELECT ${QUERY_COLS} FROM readings WHERE device_id = @device AND timestamp >= @from AND timestamp <= @to ORDER BY timestamp ASC`),
  oneDevRangeLimit: db.prepare(`SELECT ${QUERY_COLS} FROM readings WHERE device_id = @device AND timestamp >= @from AND timestamp <= @to ORDER BY timestamp ASC LIMIT @limit`),
});

const devicesStmt = db.prepare(`
  SELECT device_id, device_type, device_ip,
         MAX(timestamp) as last_seen,
         COUNT(*) as reading_count
  FROM readings
  GROUP BY device_id
  ORDER BY device_type
`);

const QUERY_COLS_ALIASED = QUERY_COLS.split(', ').map(c => `r.${c}`).join(', ');

const latestStmt = db.prepare(`
  SELECT ${QUERY_COLS_ALIASED}
  FROM readings r
  INNER JOIN (
    SELECT device_id, MAX(id) as max_id
    FROM readings
    GROUP BY device_id
  ) latest ON r.id = latest.max_id
`);

function insertReading(deviceIp, data) {
  const model = data.model || '';
  const deviceType = model.startsWith('I-') ? 'indoor' : 'outdoor';

  insertStmt.run({
    timestamp: Date.now(),
    device_id: String(data.serialno || 'unknown'),
    device_type: deviceType,
    device_ip: String(deviceIp),
    pm01: data.pm01 ?? null,
    pm02: data.pm02 ?? null,
    pm10: data.pm10 ?? null,
    pm02_compensated: data.pm02Compensated ?? null,
    rco2: data.rco2 ?? null,
    atmp: data.atmp ?? null,
    atmp_compensated: data.atmpCompensated ?? null,
    rhum: data.rhum ?? null,
    rhum_compensated: data.rhumCompensated ?? null,
    tvoc_index: data.tvocIndex ?? null,
    nox_index: data.noxIndex ?? null,
    wifi: data.wifi ?? null,
    raw_json: JSON.stringify(data),
  });
}

function queryReadings({ device, from, to, limit } = {}) {
  const wantDevice = device && device !== 'all';
  const fromN = Number(from);
  const toN = Number(to);

  if (!Number.isFinite(fromN) || !Number.isFinite(toN)) {
    return [];
  }

  const params = { from: fromN, to: toN };

  if (wantDevice) {
    params.device = String(device);
  }

  const limitN = limit != null ? Math.floor(Number(limit)) : 0;
  if (limitN > 0) {
    params.limit = limitN;
    return (wantDevice ? queryStmts.oneDevRangeLimit : queryStmts.allDevRangeLimit).all(params);
  }

  return (wantDevice ? queryStmts.oneDevRange : queryStmts.allDevRange).all(params);
}

function getDevices() {
  return devicesStmt.all();
}

function getLatestReadings() {
  return latestStmt.all();
}

function checkpoint() {
  db.pragma('wal_checkpoint(TRUNCATE)');
}

function close() {
  db.close();
}

module.exports = { insertReading, queryReadings, getDevices, getLatestReadings, checkpoint, close };
