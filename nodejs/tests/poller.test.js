'use strict';

const { describe, it, before, after, beforeEach, mock } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');

// Temp DB — must be set before requiring modules
const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'ag-poller-test-'));
const dbPath = path.join(tmpDir, 'test.db');
process.env.DB_PATH = dbPath;

const { indoorFull } = require('./fixtures');
const db = require('../src/db');
const poller = require('../src/poller');

before(() => {
  mock.method(console, 'log', () => {});
  mock.method(console, 'error', () => {});
});

after(() => {
  poller.stopPoller();
  db.close();
  mock.restoreAll();
  fs.rmSync(tmpDir, { recursive: true, force: true });
});

function mockFetchOk(data) {
  return mock.method(globalThis, 'fetch', async () => ({
    ok: true,
    status: 200,
    json: async () => data,
  }));
}

function mockFetchStatus(status) {
  return mock.method(globalThis, 'fetch', async () => ({
    ok: false,
    status,
    json: async () => ({}),
  }));
}

function mockFetchThrow(error) {
  return mock.method(globalThis, 'fetch', async () => { throw error; });
}

function mockFetchJsonBody(body) {
  return mock.method(globalThis, 'fetch', async () => ({
    ok: true,
    status: 200,
    json: async () => body,
  }));
}

describe('poller health tracking', () => {
  beforeEach(() => {
    mock.restoreAll();
    mock.method(console, 'log', () => {});
    mock.method(console, 'error', () => {});
    poller.initHealth();
  });

  it('successful poll sets health to ok', async () => {
    mockFetchOk({ ...indoorFull });
    await poller.pollAll();

    const health = poller.getHealth();
    assert.ok(health.length > 0);
    for (const h of health) {
      assert.equal(h.status, 'ok');
      assert.equal(h.consecutiveFailures, 0);
      assert.ok(h.lastSuccess > 0);
      assert.equal(h.lastErrorMessage, null);
    }
  });

  it('HTTP 500 sets health to error with status code', async () => {
    mockFetchStatus(500);
    await poller.pollAll();

    for (const h of poller.getHealth()) {
      assert.equal(h.status, 'error');
      assert.equal(h.consecutiveFailures, 1);
      assert.match(h.lastErrorMessage, /HTTP 500/);
    }
  });

  it('HTTP 404 sets health to error', async () => {
    mockFetchStatus(404);
    await poller.pollAll();

    for (const h of poller.getHealth()) {
      assert.equal(h.status, 'error');
      assert.match(h.lastErrorMessage, /HTTP 404/);
    }
  });

  it('network error sets health to error', async () => {
    mockFetchThrow(new TypeError('fetch failed'));
    await poller.pollAll();

    for (const h of poller.getHealth()) {
      assert.equal(h.status, 'error');
      assert.match(h.lastErrorMessage, /fetch failed/);
    }
  });

  it('abort/timeout sets health to error with timeout message', async () => {
    mockFetchThrow(new DOMException('The operation was aborted', 'AbortError'));
    await poller.pollAll();

    for (const h of poller.getHealth()) {
      assert.equal(h.status, 'error');
      assert.match(h.lastErrorMessage, /timeout/);
    }
  });

  it('non-object JSON response (array) sets health to error', async () => {
    mockFetchJsonBody([1, 2, 3]);
    await poller.pollAll();

    for (const h of poller.getHealth()) {
      assert.equal(h.status, 'error');
      assert.match(h.lastErrorMessage, /unexpected response type/);
    }
  });

  it('non-object JSON response (string) sets health to error', async () => {
    mockFetchJsonBody('just a string');
    await poller.pollAll();

    for (const h of poller.getHealth()) {
      assert.equal(h.status, 'error');
      assert.match(h.lastErrorMessage, /unexpected response type/);
    }
  });

  it('null JSON response sets health to error', async () => {
    mockFetchJsonBody(null);
    await poller.pollAll();

    for (const h of poller.getHealth()) {
      assert.equal(h.status, 'error');
      assert.match(h.lastErrorMessage, /unexpected response type/);
    }
  });

  it('consecutive failures increment', async () => {
    mockFetchStatus(503);
    await poller.pollAll();
    await poller.pollAll();
    await poller.pollAll();

    for (const h of poller.getHealth()) {
      assert.equal(h.consecutiveFailures, 3);
    }
  });

  it('success after failure resets consecutiveFailures', async () => {
    mockFetchStatus(500);
    await poller.pollAll();

    for (const h of poller.getHealth()) {
      assert.equal(h.consecutiveFailures, 1);
      assert.equal(h.status, 'error');
    }

    mock.restoreAll();
    mock.method(console, 'log', () => {});
    mock.method(console, 'error', () => {});
    mockFetchOk({ ...indoorFull });
    await poller.pollAll();

    for (const h of poller.getHealth()) {
      assert.equal(h.consecutiveFailures, 0);
      assert.equal(h.status, 'ok');
      assert.equal(h.lastErrorMessage, null);
    }
  });

  it('successful poll inserts reading into DB', async () => {
    mockFetchOk({ ...indoorFull, serialno: 'poller_insert_test' });
    await poller.pollAll();

    const latest = db.getLatestReadings();
    const row = latest.find(r => r.device_id === 'poller_insert_test');
    assert.ok(row, 'Reading should be inserted into DB');
    assert.equal(row.device_type, 'indoor');
  });
});

describe('initHealth', () => {
  it('sets all devices to unknown status', () => {
    poller.initHealth();
    for (const h of poller.getHealth()) {
      assert.equal(h.status, 'unknown');
      assert.equal(h.lastSuccess, null);
      assert.equal(h.lastError, null);
      assert.equal(h.lastErrorMessage, null);
      assert.equal(h.consecutiveFailures, 0);
    }
  });

  it('creates entry for each configured device', () => {
    poller.initHealth();
    const health = poller.getHealth();
    const config = require('../config');
    assert.equal(health.length, config.devices.length);
    for (const device of config.devices) {
      assert.ok(health.find(h => h.ip === device.ip), `Missing health entry for ${device.ip}`);
    }
  });
});

describe('stopPoller', () => {
  it('can be called safely when not started', () => {
    assert.doesNotThrow(() => poller.stopPoller());
  });

  it('can be called multiple times', () => {
    assert.doesNotThrow(() => {
      poller.stopPoller();
      poller.stopPoller();
    });
  });
});
