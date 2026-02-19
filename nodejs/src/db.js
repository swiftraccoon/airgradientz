'use strict';

const fs = require('node:fs');
const path = require('node:path');
const Database = require('better-sqlite3');
const config = require('../config');

const db = new Database(config.dbPath);
db.pragma('journal_mode = WAL');
db.pragma('busy_timeout = 5000');
db.pragma('foreign_keys = ON');

const schemaPath = path.join(__dirname, '..', '..', 'schema.sql');
const schema = fs.readFileSync(schemaPath, 'utf8');
db.exec(schema);

// Load named queries from shared queries.sql
function parseQueriesSql(content) {
  const queries = new Map();
  let name = null;
  let lines = [];
  for (const line of content.split('\n')) {
    const m = line.match(/^-- name: (\S+)/);
    if (m) {
      if (name) {
        queries.set(name, lines.join('\n').trim().replace(/;$/, ''));
      }
      name = m[1];
      lines = [];
    } else if (!line.startsWith('--') && line.trim()) {
      lines.push(line.trim());
    }
  }
  if (name) {
    queries.set(name, lines.join('\n').trim().replace(/;$/, ''));
  }
  return queries;
}

const queriesPath = path.join(__dirname, '..', '..', 'queries.sql');
const queries = parseQueriesSql(fs.readFileSync(queriesPath, 'utf8'));

// Column list from shared queries.sql (multiline -> single line)
const QUERY_COLS = queries.get('reading_columns').replace(/\n/g, ' ').replace(/\s+/g, ' ').trim();

const insertStmt = db.prepare(queries.get('insert_reading').replace(/:([a-z_]+)/g, '@$1'));

// Pre-prepared statements for all query variants (avoids per-request prepare leak)
const queryStmts = Object.freeze({
  allDevRange:      db.prepare(`SELECT ${QUERY_COLS} FROM readings WHERE timestamp >= @from AND timestamp <= @to ORDER BY timestamp ASC`),
  allDevRangeLimit: db.prepare(`SELECT ${QUERY_COLS} FROM readings WHERE timestamp >= @from AND timestamp <= @to ORDER BY timestamp ASC LIMIT @limit`),
  oneDevRange:      db.prepare(`SELECT ${QUERY_COLS} FROM readings WHERE device_id = @device AND timestamp >= @from AND timestamp <= @to ORDER BY timestamp ASC`),
  oneDevRangeLimit: db.prepare(`SELECT ${QUERY_COLS} FROM readings WHERE device_id = @device AND timestamp >= @from AND timestamp <= @to ORDER BY timestamp ASC LIMIT @limit`),
});

const devicesStmt = db.prepare(queries.get('select_devices'));

const latestStmt = db.prepare(queries.get('select_latest'));

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

function queryReadings({ device, from, to, limit, downsampleMs } = {}) {
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

  const limitN = limit !== undefined && limit !== null ? Math.floor(Number(limit)) : 0;

  // Downsampled query: bucket_ms is a trusted integer from config.downsampleBuckets, interpolated directly
  if (downsampleMs) {
    const bucketMs = Number(downsampleMs);
    const deviceClause = wantDevice ? 'device_id = @device AND ' : '';
    const limitClause = limitN > 0 ? ' LIMIT @limit' : '';
    if (limitN > 0) { params.limit = limitN; }

    const sql = `SELECT (timestamp / ${bucketMs}) * ${bucketMs} AS timestamp,`
      + ' device_id, device_type, device_ip,'
      + ' AVG(pm01) AS pm01, AVG(pm02) AS pm02, AVG(pm10) AS pm10,'
      + ' AVG(pm02_compensated) AS pm02_compensated,'
      + ' CAST(AVG(rco2) AS INTEGER) AS rco2,'
      + ' AVG(atmp) AS atmp, AVG(atmp_compensated) AS atmp_compensated,'
      + ' AVG(rhum) AS rhum, AVG(rhum_compensated) AS rhum_compensated,'
      + ' AVG(tvoc_index) AS tvoc_index, AVG(nox_index) AS nox_index,'
      + ' CAST(AVG(wifi) AS INTEGER) AS wifi'
      + ` FROM readings WHERE ${deviceClause}timestamp >= @from AND timestamp <= @to`
      + ` GROUP BY (timestamp / ${bucketMs}), device_id ORDER BY timestamp ASC`
      + limitClause;

    return db.prepare(sql).all(params);
  }

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

const countStmt = db.prepare(queries.get('count_readings'));

function getReadingsCount() {
  return Object.values(countStmt.get())[0];
}

const filteredCountStmts = Object.freeze({
  allDev: db.prepare('SELECT COUNT(*) AS count FROM readings WHERE timestamp >= @from AND timestamp <= @to'),
  oneDev: db.prepare('SELECT COUNT(*) AS count FROM readings WHERE device_id = @device AND timestamp >= @from AND timestamp <= @to'),
});

function getFilteredCount({ from, to, device } = {}) {
  const fromN = Number(from);
  const toN = Number(to);

  if (!Number.isFinite(fromN) || !Number.isFinite(toN)) {
    return 0;
  }

  const wantDevice = device && device !== 'all';
  const params = { from: fromN, to: toN };

  if (wantDevice) {
    params.device = String(device);
    return filteredCountStmts.oneDev.get(params).count;
  }

  return filteredCountStmts.allDev.get(params).count;
}

function checkpoint() {
  db.pragma('wal_checkpoint(TRUNCATE)');
}

function close() {
  db.close();
}

module.exports = { insertReading, queryReadings, getDevices, getLatestReadings, getReadingsCount, getFilteredCount, checkpoint, close };
