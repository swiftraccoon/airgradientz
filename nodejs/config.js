'use strict';

const fs = require('node:fs');
const path = require('node:path');
const { timestamp } = require('./src/log');

function loadConfigFile() {
  const candidates = [
    process.env.CONFIG_PATH,
    path.join(process.cwd(), 'airgradientz.json'),
    path.join(process.cwd(), '..', 'airgradientz.json'),
  ].filter(Boolean);

  for (const p of candidates) {
    try {
      // Path `p` comes from a controlled list: CONFIG_PATH env var (operator-set),
      // or hardcoded ./airgradientz.json and ../airgradientz.json. Not user input.
      const content = fs.readFileSync(p, 'utf8'); // eslint-disable-line security/detect-non-literal-fs-filename
      const label = p === process.env.CONFIG_PATH ? `CONFIG_PATH: ${p}` : p;
      console.error(`${timestamp()} [config] Loaded config from ${label}`);
      return JSON.parse(content);
    } catch {
      if (p === process.env.CONFIG_PATH) {
        console.error(`${timestamp()} [config] CONFIG_PATH set but unreadable: ${p}`);
      }
    }
  }
  return null;
}

const fileConfig = loadConfigFile();
if (!fileConfig) {
  console.error('fatal: config file not found (searched CONFIG_PATH, ./airgradientz.json, ../airgradientz.json)');
  process.exit(1);
}

// Validate required keys
const missing = [];
if (typeof fileConfig.pollIntervalMs !== 'number' || fileConfig.pollIntervalMs <= 0) {
  missing.push('pollIntervalMs');
}
if (typeof fileConfig.fetchTimeoutMs !== 'number' || fileConfig.fetchTimeoutMs <= 0) {
  missing.push('fetchTimeoutMs');
}
if (typeof fileConfig.maxApiRows !== 'number' || fileConfig.maxApiRows <= 0) {
  missing.push('maxApiRows');
}
if (!fileConfig.downsampleBuckets || typeof fileConfig.downsampleBuckets !== 'object' || Object.keys(fileConfig.downsampleBuckets).length === 0) {
  missing.push('downsampleBuckets');
}
if (!Array.isArray(fileConfig.devices) || fileConfig.devices.length === 0) {
  missing.push('devices');
} else if (fileConfig.devices.some(d => !d.ip || !d.label)) {
  missing.push('devices (each entry needs ip and label)');
}
if (fileConfig.ports?.nodejs === undefined || fileConfig.ports?.nodejs === null) {
  missing.push('ports.nodejs');
}
if (missing.length > 0) {
  console.error(`fatal: missing required config keys: ${missing.join(', ')}`);
  process.exit(1);
}

// Read values directly from config file
const merged = {
  port: Number(fileConfig.ports.nodejs),
  dbPath: process.env.DB_PATH || path.join(__dirname, 'airgradientz.db'),
  devices: fileConfig.devices,
  pollIntervalMs: fileConfig.pollIntervalMs,
  fetchTimeoutMs: fileConfig.fetchTimeoutMs,
  maxApiRows: fileConfig.maxApiRows,
  downsampleBuckets: fileConfig.downsampleBuckets,
  shutdownTimeoutMs: 5_000,
};

// PORT env var overrides (highest priority)
if (process.env.PORT) {
  const port = Number(process.env.PORT);
  if (port > 0 && port <= 65535) {
    merged.port = port;
  }
}

// Freeze everything
merged.devices = Object.freeze(merged.devices.map(d => Object.freeze(d)));
Object.freeze(merged.downsampleBuckets);
const config = Object.freeze(merged);

module.exports = config;
