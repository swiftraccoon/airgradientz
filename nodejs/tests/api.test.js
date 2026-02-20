'use strict';

const { describe, it, before, after, mock } = require('node:test');
const assert = require('node:assert/strict');
const http = require('node:http');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');

// Temp DB — must be set before requiring modules
const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'ag-api-test-'));
const dbPath = path.join(tmpDir, 'test.db');
process.env.DB_PATH = dbPath;

const db = require('../src/db');

mock.method(console, 'log', () => {});
mock.method(console, 'error', () => {});

const poller = require('../src/poller');
poller.initHealth();

const { handleApi, sendJson } = require('../src/api');

let server;
let baseUrl;

function parseQuery(url) {
  const params = {};
  const qIdx = url.indexOf('?');
  if (qIdx === -1) {
    return params;
  }
  const searchParams = new URLSearchParams(url.slice(qIdx + 1));
  for (const [key, value] of searchParams) {
    params[key] = value;
  }
  return params;
}

function getPathname(url) {
  const qIdx = url.indexOf('?');
  return qIdx === -1 ? url : url.slice(0, qIdx);
}

const startedAt = Date.now();
let requestsServed = 0;
const appLocals = {
  startedAt,
  getRequestsServed: () => requestsServed,
};

before(async () => {
  const now = Date.now();
  const Database = require('better-sqlite3');
  const rawDb = new Database(dbPath);
  const stmt = rawDb.prepare(`
    INSERT INTO readings (timestamp, device_id, device_type, device_ip,
      pm02, pm02_compensated, rco2, atmp, atmp_compensated, rhum, rhum_compensated,
      tvoc_index, nox_index, wifi, raw_json)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  `);
  for (let i = 0; i < 20; i++) {
    stmt.run(
      now - (20 - i) * 60_000,
      'indoor_test', 'indoor', '10.0.0.1',
      10 + i, 8 + i, 400 + i * 5, 20 + i * 0.1, 19.5 + i * 0.1,
      50 + i * 0.5, 52 + i * 0.5, 100 + i, 1, -50, '{"test":true}',
    );
    stmt.run(
      now - (20 - i) * 60_000,
      'outdoor_test', 'outdoor', '10.0.0.2',
      20 + i, 15 + i, 430 + i * 5, 10 + i * 0.1, 7 + i * 0.1,
      35 + i * 0.5, 48 + i * 0.5, 200 + i, 2, -40, '{"test":true}',
    );
  }
  rawDb.close();

  server = http.createServer((req, res) => {
    requestsServed++;
    const pathname = getPathname(req.url);
    const query = parseQuery(req.url);

    if (pathname.startsWith('/api/')) {
      const apiPath = pathname.slice(4);
      const handled = handleApi(req, res, apiPath, query, appLocals);
      if (!handled) {
        sendJson(res, 404, { error: 'Not found' });
      }
      return;
    }
    sendJson(res, 404, { error: 'Not found' });
  });

  await new Promise((resolve) => {
    server.listen(0, '127.0.0.1', () => {
      const addr = server.address();
      baseUrl = `http://127.0.0.1:${addr.port}`;
      resolve();
    });
  });
});

after(async () => {
  await new Promise((resolve) => server.close(resolve));
  db.close();
  mock.restoreAll();
  fs.rmSync(tmpDir, { recursive: true, force: true });
});

async function get(urlPath) {
  const res = await fetch(`${baseUrl}${urlPath}`);
  const body = await res.json();
  return { status: res.status, headers: res.headers, body };
}

describe('GET /api/readings', () => {
  it('returns a JSON array', async () => {
    const { status, body } = await get('/api/readings');
    assert.equal(status, 200);
    assert.ok(Array.isArray(body));
  });

  it('returns readings by default (last 24h)', async () => {
    const { body } = await get('/api/readings');
    assert.ok(body.length > 0);
  });

  it('does NOT include raw_json in response', async () => {
    const { body } = await get('/api/readings');
    for (const r of body) {
      assert.equal('raw_json' in r, false, 'raw_json should not appear');
    }
  });

  it('includes compensated value columns', async () => {
    const { body } = await get('/api/readings');
    const row = body[0];
    assert.ok('pm02_compensated' in row);
    assert.ok('atmp_compensated' in row);
    assert.ok('rhum_compensated' in row);
  });

  it('respects limit parameter', async () => {
    const { body } = await get('/api/readings?limit=3');
    assert.equal(body.length, 3);
  });

  it('bad limit defaults to MAX_ROWS cap', async () => {
    const { body: withBad } = await get('/api/readings?limit=foo');
    const { body: withoutLimit } = await get('/api/readings');
    assert.equal(withBad.length, withoutLimit.length);
  });

  it('negative limit defaults to MAX_ROWS cap', async () => {
    const { body } = await get('/api/readings?limit=-10');
    assert.ok(body.length > 0);
  });

  it('filters by device', async () => {
    const { body } = await get('/api/readings?device=indoor_test');
    assert.ok(body.length > 0);
    assert.ok(body.every(r => r.device_id === 'indoor_test'));
  });

  it('from/to restricts time range', async () => {
    const now = Date.now();
    const from = now - 5 * 60_000;
    const { body } = await get(`/api/readings?from=${from}&to=${now}`);
    assert.ok(body.length > 0);
    for (const r of body) {
      assert.ok(r.timestamp >= from);
      assert.ok(r.timestamp <= now);
    }
  });

  it('future time range returns empty', async () => {
    const now = Date.now();
    const { body } = await get(`/api/readings?from=${now + 60_000}&to=${now + 120_000}`);
    assert.equal(body.length, 0);
  });

  it('results are ordered by timestamp ASC', async () => {
    const { body } = await get('/api/readings');
    for (let i = 1; i < body.length; i++) {
      assert.ok(body[i].timestamp >= body[i - 1].timestamp);
    }
  });
});

describe('GET /api/devices', () => {
  it('returns a JSON array of devices', async () => {
    const { status, body } = await get('/api/devices');
    assert.equal(status, 200);
    assert.ok(Array.isArray(body));
    assert.ok(body.length > 0);
  });

  it('each device has expected fields', async () => {
    const { body } = await get('/api/devices');
    for (const d of body) {
      assert.ok(d.device_id);
      assert.ok(d.device_type);
      assert.ok(d.device_ip);
      assert.ok(typeof d.last_seen === 'number');
      assert.ok(typeof d.reading_count === 'number');
    }
  });

  it('returns unique device_ids', async () => {
    const { body } = await get('/api/devices');
    const ids = body.map(d => d.device_id);
    assert.equal(ids.length, new Set(ids).size);
  });

  it('does NOT contain first_seen field (regression)', async () => {
    const { body } = await get('/api/devices');
    for (const d of body) {
      assert.equal('first_seen' in d, false, 'devices response should not have first_seen');
    }
  });
});

describe('GET /api/readings/latest', () => {
  it('returns one reading per device', async () => {
    const { status, body } = await get('/api/readings/latest');
    assert.equal(status, 200);
    assert.ok(Array.isArray(body));
    const ids = body.map(r => r.device_id);
    assert.equal(ids.length, new Set(ids).size);
  });

  it('does NOT include raw_json', async () => {
    const { body } = await get('/api/readings/latest');
    for (const r of body) {
      assert.equal('raw_json' in r, false);
    }
  });

  it('returns the most recent reading per device', async () => {
    const { body: latest } = await get('/api/readings/latest');
    const { body: all } = await get('/api/readings');
    for (const reading of latest) {
      const deviceReadings = all.filter(r => r.device_id === reading.device_id);
      const maxTs = Math.max(...deviceReadings.map(r => r.timestamp));
      assert.equal(reading.timestamp, maxTs);
    }
  });
});

describe('GET /api/readings/count', () => {
  it('returns only {"count": N} with no extra fields (regression)', async () => {
    const { status, body } = await get('/api/readings/count');
    assert.equal(status, 200);
    assert.ok(typeof body.count === 'number');
    const keys = Object.keys(body);
    assert.deepEqual(keys, ['count'], `Expected only "count" key, got: ${JSON.stringify(keys)}`);
  });
});

describe('GET /api/health', () => {
  it('returns a JSON array', async () => {
    const { status, body } = await get('/api/health');
    assert.equal(status, 200);
    assert.ok(Array.isArray(body));
  });

  it('each entry has expected health fields', async () => {
    const { body } = await get('/api/health');
    for (const h of body) {
      assert.ok('ip' in h);
      assert.ok('label' in h);
      assert.ok('status' in h);
      assert.ok(['ok', 'error', 'unknown'].includes(h.status));
      assert.ok('consecutiveFailures' in h);
      assert.ok(typeof h.consecutiveFailures === 'number');
    }
  });

  it('has entry for each configured device', async () => {
    const config = require('../config');
    const { body } = await get('/api/health');
    assert.equal(body.length, config.devices.length);
  });
});

describe('GET /api/config', () => {
  it('returns poll interval and device list', async () => {
    const config = require('../config');
    const { status, body } = await get('/api/config');
    assert.equal(status, 200);
    assert.equal(body.pollIntervalMs, config.pollIntervalMs);
    assert.ok(Array.isArray(body.devices));
    assert.equal(body.devices.length, config.devices.length);
  });

  it('device list contains ip and label only', async () => {
    const { body } = await get('/api/config');
    for (const d of body.devices) {
      assert.ok(d.ip);
      assert.ok(d.label);
      assert.deepEqual(Object.keys(d).sort(), ['ip', 'label']);
    }
  });
});

describe('security headers', () => {
  it('does not expose x-powered-by', async () => {
    const { headers } = await get('/api/health');
    assert.equal(headers.get('x-powered-by'), null);
  });
});
