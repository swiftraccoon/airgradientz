'use strict';

const fs = require('node:fs');
const { timestamp } = require('./log');
const { queryReadings, getDevices, getLatestReadings, getReadingsCount, getFilteredCount } = require('./db');
const { getHealth, getPollStats } = require('./poller');
const config = require('../config');

function sendJson(res, statusCode, data) {
  const body = JSON.stringify(data);
  res.writeHead(statusCode, {
    'Content-Type': 'application/json',
    'Content-Length': Buffer.byteLength(body),
  });
  res.end(body);
}

/**
 * Handle API requests. pathname should already have /api prefix stripped.
 * Returns true if the route was handled, false otherwise.
 */
function handleApi(req, res, pathname, query, appLocals) {
  try {
    if (pathname === '/readings') {
      return handleReadings(res, query);
    }
    if (pathname === '/readings/latest') {
      return handleLatest(res);
    }
    if (pathname === '/readings/count') {
      return handleCount(res, query);
    }
    if (pathname === '/devices') {
      return handleDevices(res);
    }
    if (pathname === '/health') {
      return handleHealth(res);
    }
    if (pathname === '/config') {
      return handleConfig(res);
    }
    if (pathname === '/stats') {
      return handleStats(res, appLocals);
    }
    return false;
  } catch (err) {
    console.error(`${timestamp()} [api] Unhandled error:`, err);
    sendJson(res, 500, { error: 'Internal server error' });
    return true;
  }
}

function handleReadings(res, query) {
  const { device, from, to, limit, downsample } = query;

  if (downsample !== undefined) {
    if (!Object.hasOwn(config.downsampleBuckets, downsample)) {
      sendJson(res, 400, { error: 'invalid downsample value' });
      return true;
    }
  }

  const now = Date.now();
  const effectiveFrom = from || String(now - 24 * 60 * 60 * 1000);
  const effectiveTo = to || String(now);

  const requestedLimit = limit ? Number(limit) : config.maxApiRows;
  const effectiveLimit = Math.min(
    Number.isFinite(requestedLimit) && requestedLimit > 0 ? requestedLimit : config.maxApiRows,
    config.maxApiRows,
  );

  const readings = queryReadings({
    device: device || 'all',
    from: effectiveFrom,
    to: effectiveTo,
    limit: effectiveLimit,
    // eslint-disable-next-line security/detect-object-injection -- validated via Object.hasOwn above
    downsampleMs: downsample && Object.hasOwn(config.downsampleBuckets, downsample) ? config.downsampleBuckets[downsample] : undefined,
  });

  sendJson(res, 200, readings);
  return true;
}

function handleLatest(res) {
  sendJson(res, 200, getLatestReadings());
  return true;
}

function handleCount(res, query) {
  const { device, from, to } = query;

  const now = Date.now();
  const effectiveFrom = from || '0';
  const effectiveTo = to || String(now);

  const count = getFilteredCount({
    from: effectiveFrom,
    to: effectiveTo,
    device,
  });

  sendJson(res, 200, { count });
  return true;
}

function handleDevices(res) {
  sendJson(res, 200, getDevices());
  return true;
}

function handleHealth(res) {
  sendJson(res, 200, getHealth());
  return true;
}

function handleConfig(res) {
  sendJson(res, 200, {
    pollIntervalMs: config.pollIntervalMs,
    downsampleBuckets: config.downsampleBuckets,
    devices: config.devices.map(d => ({ ip: d.ip, label: d.label })),
  });
  return true;
}

function handleStats(res, appLocals) {
  const { pollSuccesses, pollFailures } = getPollStats();
  let dbSizeBytes = 0;
  try {
    // config.dbPath comes from: DB_PATH env var (operator-set) or hardcoded default.
    // Not user-controlled input.
    dbSizeBytes = fs.statSync(config.dbPath).size; // eslint-disable-line security/detect-non-literal-fs-filename
  } catch {}

  sendJson(res, 200, {
    implementation: 'nodejs',
    pid: process.pid,
    uptime_ms: Date.now() - appLocals.startedAt,
    memory_rss_bytes: process.memoryUsage.rss(),
    db_size_bytes: dbSizeBytes,
    readings_count: getReadingsCount(),
    requests_served: appLocals.getRequestsServed(),
    active_connections: 0,
    poll_successes: pollSuccesses,
    poll_failures: pollFailures,
    started_at: appLocals.startedAt,
  });
  return true;
}

module.exports = { handleApi, sendJson };
