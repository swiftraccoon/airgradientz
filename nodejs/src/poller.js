'use strict';

const { insertReading, checkpoint } = require('./db');
const config = require('../config');

const CHECKPOINT_INTERVAL_MS = 5 * 60 * 1000;

let pollIntervalId = null;
let checkpointIntervalId = null;

// Per-device health state, keyed by IP
const health = new Map();

function initHealth() {
  health.clear();
  for (const device of config.devices) {
    health.set(device.ip, {
      ip: device.ip,
      label: device.label,
      status: 'unknown',
      lastSuccess: null,
      lastError: null,
      lastErrorMessage: null,
      consecutiveFailures: 0,
    });
  }
}

async function fetchDevice(device) {
  const url = `http://${device.ip}/measures/current`;
  const h = health.get(device.ip);
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), config.fetchTimeoutMs);
  try {
    const res = await fetch(url, { signal: controller.signal });
    if (!res.ok) {
      const msg = `HTTP ${res.status}`;
      recordFailure(h, msg);
      console.error(`[poller] ${device.label} (${device.ip}): ${msg}`);
      return;
    }
    // Keep timeout active through body read — if body stalls, abort fires
    const data = await res.json();
    clearTimeout(timeout);

    if (typeof data !== 'object' || data === null || Array.isArray(data)) {
      const msg = `unexpected response type: ${typeof data}`;
      recordFailure(h, msg);
      console.error(`[poller] ${device.label} (${device.ip}): ${msg}`);
      return;
    }

    try {
      insertReading(device.ip, data);
    } catch (dbErr) {
      const msg = `DB insert failed: ${dbErr.message}`;
      recordFailure(h, msg);
      console.error(`[poller] ${device.label} (${device.ip}): ${msg}`);
      return;
    }

    recordSuccess(h);
    console.log(`[poller] ${device.label} (${device.ip}): OK — PM2.5=${data.pm02}, CO2=${data.rco2}, T=${data.atmp}°C`);
  } catch (err) {
    clearTimeout(timeout);
    const msg = err.name === 'AbortError' ? `timeout after ${config.fetchTimeoutMs}ms` : err.message;
    recordFailure(h, msg);
    console.error(`[poller] ${device.label} (${device.ip}): fetch failed: ${msg}`);
  }
}

function recordSuccess(h) {
  h.status = 'ok';
  h.lastSuccess = Date.now();
  h.lastErrorMessage = null;
  h.consecutiveFailures = 0;
}

function recordFailure(h, message) {
  h.status = 'error';
  h.lastError = Date.now();
  h.lastErrorMessage = message;
  h.consecutiveFailures++;
}

async function pollAll() {
  await Promise.allSettled(config.devices.map(fetchDevice));
}

function getHealth() {
  return Array.from(health.values());
}

function startPoller() {
  console.log(`[poller] Starting — polling ${config.devices.length} devices every ${config.pollIntervalMs / 1000}s`);
  initHealth();
  pollAll();
  pollIntervalId = setInterval(pollAll, config.pollIntervalMs);
  checkpointIntervalId = setInterval(() => {
    try { checkpoint(); } catch (err) {
      console.error(`[poller] WAL checkpoint failed: ${err.message}`);
    }
  }, CHECKPOINT_INTERVAL_MS);
}

function stopPoller() {
  if (pollIntervalId !== null) {
    clearInterval(pollIntervalId);
    pollIntervalId = null;
  }
  if (checkpointIntervalId !== null) {
    clearInterval(checkpointIntervalId);
    checkpointIntervalId = null;
  }
  console.log('[poller] Stopped');
}

module.exports = { startPoller, stopPoller, getHealth, initHealth, pollAll };
