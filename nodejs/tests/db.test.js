'use strict';

const { describe, it, before, after } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');

// Temp DB — must be set before requiring db module
const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'ag-db-test-'));
const dbPath = path.join(tmpDir, 'test.db');
process.env.DB_PATH = dbPath;

const db = require('../src/db');
const { indoorFull, outdoorFull, afterBoot, zeroCompensated } = require('./fixtures');

after(() => {
  db.close();
  fs.rmSync(tmpDir, { recursive: true, force: true });
});

describe('insertReading', () => {
  it('inserts a full indoor reading', () => {
    assert.doesNotThrow(() => db.insertReading('192.168.88.159', { ...indoorFull }));
  });

  it('inserts a full outdoor reading', () => {
    assert.doesNotThrow(() => db.insertReading('192.168.88.6', { ...outdoorFull }));
  });

  it('handles post-boot data with null sensor fields', () => {
    assert.doesNotThrow(() => db.insertReading('192.168.88.159', { ...afterBoot }));
  });

  it('handles zero compensated values (zero is valid, not null)', () => {
    assert.doesNotThrow(() => db.insertReading('192.168.88.159', { ...zeroCompensated }));
  });

  it('classifies I-* models as indoor', () => {
    db.insertReading('10.0.0.1', { ...indoorFull, serialno: 'test_indoor', model: 'I-9PSL-DE' });
    const latest = db.getLatestReadings().find(r => r.device_id === 'test_indoor');
    assert.equal(latest.device_type, 'indoor');
  });

  it('classifies O-* models as outdoor', () => {
    db.insertReading('10.0.0.2', { ...outdoorFull, serialno: 'test_outdoor', model: 'O-1PST' });
    const latest = db.getLatestReadings().find(r => r.device_id === 'test_outdoor');
    assert.equal(latest.device_type, 'outdoor');
  });

  it('defaults unknown models to outdoor', () => {
    db.insertReading('10.0.0.3', { ...indoorFull, serialno: 'test_unknown', model: 'DIY-PRO' });
    const latest = db.getLatestReadings().find(r => r.device_id === 'test_unknown');
    assert.equal(latest.device_type, 'outdoor');
  });

  it('defaults missing serialno to "unknown"', () => {
    const data = { ...indoorFull };
    delete data.serialno;
    db.insertReading('10.0.0.4', data);
    const latest = db.getLatestReadings().find(r => r.device_ip === '10.0.0.4');
    assert.equal(latest.device_id, 'unknown');
  });

  it('stores raw_json as stringified original data', () => {
    const Database = require('better-sqlite3');
    const rawDb = new Database(dbPath, { readonly: true });
    const row = rawDb.prepare('SELECT raw_json FROM readings WHERE device_id = ?').get('test_indoor');
    rawDb.close();
    const parsed = JSON.parse(row.raw_json);
    assert.equal(parsed.model, 'I-9PSL-DE');
    assert.equal(parsed.serialno, 'test_indoor');
  });

  it('rejects invalid device_type via CHECK constraint', () => {
    const Database = require('better-sqlite3');
    const rawDb = new Database(dbPath);
    assert.throws(() => {
      rawDb.prepare(`
        INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json)
        VALUES (?, ?, ?, ?, ?)
      `).run(Date.now(), 'x', 'INVALID', '0.0.0.0', '{}');
    }, /CHECK constraint/);
    rawDb.close();
  });
});

describe('queryReadings', () => {
  before(() => {
    const now = Date.now();
    const Database = require('better-sqlite3');
    const rawDb = new Database(dbPath);
    const stmt = rawDb.prepare(`
      INSERT INTO readings (timestamp, device_id, device_type, device_ip,
        pm02, rco2, atmp, raw_json)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    `);
    for (let i = 0; i < 5; i++) {
      stmt.run(now - (5 - i) * 60_000, 'query_test_indoor', 'indoor', '10.0.0.10', 10 + i, 400 + i * 10, 20 + i, '{}');
      stmt.run(now - (5 - i) * 60_000, 'query_test_outdoor', 'outdoor', '10.0.0.11', 20 + i, 430 + i * 10, 10 + i, '{}');
    }
    rawDb.close();
  });

  it('returns readings within time range', () => {
    const now = Date.now();
    const results = db.queryReadings({ from: String(now - 10 * 60_000), to: String(now) });
    assert.ok(results.length > 0);
  });

  it('excludes readings outside time range', () => {
    const now = Date.now();
    const results = db.queryReadings({ from: String(now + 60_000), to: String(now + 120_000) });
    assert.equal(results.length, 0);
  });

  it('filters by device_id', () => {
    const now = Date.now();
    const results = db.queryReadings({ device: 'query_test_indoor', from: String(now - 10 * 60_000), to: String(now) });
    assert.ok(results.length > 0);
    assert.ok(results.every(r => r.device_id === 'query_test_indoor'));
  });

  it('device=all returns all devices', () => {
    const now = Date.now();
    const results = db.queryReadings({ device: 'all', from: String(now - 10 * 60_000), to: String(now) });
    const deviceIds = new Set(results.map(r => r.device_id));
    assert.ok(deviceIds.size > 1);
  });

  it('respects limit parameter', () => {
    const now = Date.now();
    const results = db.queryReadings({ from: String(now - 10 * 60_000), to: String(now), limit: 2 });
    assert.equal(results.length, 2);
  });

  it('limit=0 means no limit (returns all)', () => {
    const now = Date.now();
    const all = db.queryReadings({ from: String(now - 10 * 60_000), to: String(now) });
    const withZero = db.queryReadings({ from: String(now - 10 * 60_000), to: String(now), limit: 0 });
    assert.equal(all.length, withZero.length);
  });

  it('negative limit treated as no limit', () => {
    const now = Date.now();
    const all = db.queryReadings({ from: String(now - 10 * 60_000), to: String(now) });
    const withNeg = db.queryReadings({ from: String(now - 10 * 60_000), to: String(now), limit: -5 });
    assert.equal(all.length, withNeg.length);
  });

  it('returns [] for NaN from', () => {
    assert.deepEqual(db.queryReadings({ from: 'not-a-number', to: String(Date.now()) }), []);
  });

  it('returns [] for NaN to', () => {
    assert.deepEqual(db.queryReadings({ from: String(Date.now() - 60_000), to: 'garbage' }), []);
  });

  it('results are ordered by timestamp ASC', () => {
    const now = Date.now();
    const results = db.queryReadings({ from: String(now - 10 * 60_000), to: String(now) });
    for (let i = 1; i < results.length; i++) {
      assert.ok(results[i].timestamp >= results[i - 1].timestamp,
        `row ${i} timestamp ${results[i].timestamp} < previous ${results[i - 1].timestamp}`);
    }
  });

  it('response does NOT include raw_json', () => {
    const now = Date.now();
    const results = db.queryReadings({ from: String(now - 10 * 60_000), to: String(now) });
    assert.ok(results.length > 0);
    for (const r of results) {
      assert.equal('raw_json' in r, false, 'raw_json should not be in query response');
    }
  });
});

describe('getDevices', () => {
  it('returns aggregated device info', () => {
    const devices = db.getDevices();
    assert.ok(Array.isArray(devices));
    assert.ok(devices.length > 0);
    for (const d of devices) {
      assert.ok(d.device_id);
      assert.ok(d.device_type);
      assert.ok(d.device_ip);
      assert.ok(typeof d.last_seen === 'number');
      assert.ok(typeof d.reading_count === 'number');
      assert.ok(d.reading_count > 0);
    }
  });

  it('returns unique device_ids', () => {
    const devices = db.getDevices();
    const ids = devices.map(d => d.device_id);
    assert.equal(ids.length, new Set(ids).size);
  });

  it('does NOT contain first_seen field (regression)', () => {
    const devices = db.getDevices();
    assert.ok(devices.length > 0);
    for (const d of devices) {
      assert.equal('first_seen' in d, false, 'devices response should not have first_seen');
    }
  });
});

describe('getLatestReadings', () => {
  it('returns one reading per device', () => {
    const latest = db.getLatestReadings();
    assert.ok(Array.isArray(latest));
    const ids = latest.map(r => r.device_id);
    assert.equal(ids.length, new Set(ids).size);
  });

  it('returns the most recent reading for each device', () => {
    const latest = db.getLatestReadings();
    for (const reading of latest) {
      const all = db.queryReadings({ device: reading.device_id, from: '0', to: String(Date.now() + 60_000) });
      const maxTs = Math.max(...all.map(r => r.timestamp));
      assert.equal(reading.timestamp, maxTs,
        `Latest reading for ${reading.device_id} should have max timestamp`);
    }
  });

  it('selects by MAX(id) not MAX(timestamp) (regression)', () => {
    // Insert two readings for same device with identical timestamp
    const now = Date.now();
    const Database = require('better-sqlite3');
    const rawDb = new Database(dbPath);
    rawDb.prepare(`
      INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, rco2, raw_json)
      VALUES (?, ?, ?, ?, ?, ?, ?)
    `).run(now, 'maxid_test', 'indoor', '10.0.0.99', 10.0, 400, '{}');
    rawDb.prepare(`
      INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, rco2, raw_json)
      VALUES (?, ?, ?, ?, ?, ?, ?)
    `).run(now, 'maxid_test', 'indoor', '10.0.0.99', 99.0, 999, '{}');
    rawDb.close();

    const latest = db.getLatestReadings().find(r => r.device_id === 'maxid_test');
    assert.ok(latest, 'maxid_test device should appear in latest');
    assert.equal(latest.pm02, 99.0, 'Latest should be the second insert (higher id)');
    assert.equal(latest.rco2, 999);
  });

  it('response does NOT include raw_json', () => {
    const latest = db.getLatestReadings();
    for (const r of latest) {
      assert.equal('raw_json' in r, false);
    }
  });
});

describe('zero compensated values', () => {
  it('stores 0 as 0, not as null', () => {
    const results = db.queryReadings({ device: indoorFull.serialno, from: '0', to: String(Date.now() + 60_000) });
    const zeroRow = results.find(r => r.pm02_compensated === 0);
    assert.ok(zeroRow, 'Should have a row with pm02_compensated = 0');
    assert.strictEqual(zeroRow.pm02_compensated, 0);
    assert.strictEqual(zeroRow.atmp_compensated, 0);
    assert.strictEqual(zeroRow.rhum_compensated, 0);
  });
});

describe('queryReadings with downsample', () => {
  before(() => {
    const Database = require('better-sqlite3');
    const rawDb = new Database(dbPath);
    const stmt = rawDb.prepare(`
      INSERT INTO readings (timestamp, device_id, device_type, device_ip,
        pm02, rco2, atmp, wifi, raw_json)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    `);
    // Insert 6 readings within the same hour bucket (bucket = floor(ts / 3600000) * 3600000)
    const baseTs = 3_600_000 * 1000; // a clean hour boundary
    for (let i = 0; i < 6; i++) {
      stmt.run(baseTs + i * 60_000, 'ds_test_device', 'indoor', '10.0.0.20', 10 + i, 400 + i * 10, 20.0 + i, -50 + i, '{}');
    }
    // Insert 2 readings in the next hour bucket
    for (let i = 0; i < 2; i++) {
      stmt.run(baseTs + 3_600_000 + i * 60_000, 'ds_test_device', 'indoor', '10.0.0.20', 30 + i, 500 + i * 10, 26.0 + i, -44 + i, '{}');
    }
    rawDb.close();
  });

  it('groups readings within the same hour into one row', () => {
    const baseTs = 3_600_000 * 1000;
    const results = db.queryReadings({
      device: 'ds_test_device',
      from: String(baseTs),
      to: String(baseTs + 2 * 3_600_000),
      downsampleMs: 3_600_000,
    });
    assert.equal(results.length, 2, 'Should have 2 buckets (one per hour)');
  });

  it('downsampled rows have no id field', () => {
    const baseTs = 3_600_000 * 1000;
    const results = db.queryReadings({
      device: 'ds_test_device',
      from: String(baseTs),
      to: String(baseTs + 2 * 3_600_000),
      downsampleMs: 3_600_000,
    });
    assert.ok(results.length > 0);
    for (const r of results) {
      assert.equal('id' in r, false, 'downsampled rows should not have id field');
    }
  });

  it('downsampled values are averages', () => {
    const baseTs = 3_600_000 * 1000;
    const results = db.queryReadings({
      device: 'ds_test_device',
      from: String(baseTs),
      to: String(baseTs + 3_600_000 - 1),
      downsampleMs: 3_600_000,
    });
    assert.equal(results.length, 1);
    // pm02 values were 10,11,12,13,14,15 -> avg = 12.5
    assert.ok(Math.abs(results[0].pm02 - 12.5) < 0.01, `Expected pm02 ~12.5, got ${results[0].pm02}`);
    // rco2 values were 400,410,420,430,440,450 -> avg = 425, cast to integer
    assert.equal(results[0].rco2, 425);
    // wifi values were -50,-49,-48,-47,-46,-45 -> avg = -47.5, cast to integer -> -47
    assert.equal(results[0].wifi, -47);
  });
});

describe('getFilteredCount', () => {
  it('returns correct total count for time range', () => {
    const now = Date.now();
    const count = db.getFilteredCount({ from: '0', to: String(now + 60_000) });
    assert.ok(typeof count === 'number');
    assert.ok(count > 0);
  });

  it('returns 0 for empty time range', () => {
    const count = db.getFilteredCount({ from: String(Date.now() + 100_000), to: String(Date.now() + 200_000) });
    assert.equal(count, 0);
  });

  it('filters by device_id', () => {
    const now = Date.now();
    const allCount = db.getFilteredCount({ from: '0', to: String(now + 60_000) });
    const deviceCount = db.getFilteredCount({ from: '0', to: String(now + 60_000), device: 'ds_test_device' });
    assert.ok(deviceCount > 0);
    assert.ok(deviceCount <= allCount);
    assert.equal(deviceCount, 8, 'ds_test_device should have exactly 8 readings');
  });

  it('returns 0 for NaN from', () => {
    assert.equal(db.getFilteredCount({ from: 'bad', to: String(Date.now()) }), 0);
  });
});

describe('checkpoint', () => {
  it('does not throw', () => {
    assert.doesNotThrow(() => db.checkpoint());
  });
});
