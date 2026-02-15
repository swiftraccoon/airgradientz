'use strict';

const fs = require('node:fs');
const express = require('express');
const { queryReadings, getDevices, getLatestReadings, getReadingsCount } = require('./db');
const { getHealth, getPollStats } = require('./poller');
const config = require('../config');

const router = express.Router();

router.get('/readings', (req, res, next) => {
  try {
    const { device, from, to, limit } = req.query;

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
    });

    res.json(readings);
  } catch (err) {
    next(err);
  }
});

router.get('/devices', (_req, res, next) => {
  try {
    res.json(getDevices());
  } catch (err) {
    next(err);
  }
});

router.get('/readings/latest', (_req, res, next) => {
  try {
    res.json(getLatestReadings());
  } catch (err) {
    next(err);
  }
});

router.get('/health', (_req, res) => {
  res.json(getHealth());
});

router.get('/config', (_req, res) => {
  res.json({
    pollIntervalMs: config.pollIntervalMs,
    devices: config.devices.map(d => ({ ip: d.ip, label: d.label })),
  });
});

router.get('/stats', (req, res) => {
  const { pollSuccesses, pollFailures } = getPollStats();
  let dbSizeBytes = 0;
  try {
    dbSizeBytes = fs.statSync(config.dbPath).size;
  } catch {}

  res.json({
    implementation: 'nodejs',
    pid: process.pid,
    uptime_ms: Date.now() - req.app.locals.startedAt,
    memory_rss_bytes: process.memoryUsage.rss(),
    db_size_bytes: dbSizeBytes,
    readings_count: getReadingsCount(),
    requests_served: req.app.locals.getRequestsServed(),
    active_connections: 0,
    poll_successes: pollSuccesses,
    poll_failures: pollFailures,
    started_at: req.app.locals.startedAt,
  });
});

// JSON error handler — all DB or unexpected errors return JSON, not HTML
// eslint-disable-next-line no-unused-vars
router.use((err, _req, res, _next) => {
  console.error('[api] Unhandled error:', err);
  res.status(500).json({ error: 'Internal server error' });
});

module.exports = router;
