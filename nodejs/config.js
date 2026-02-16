'use strict';

const fs = require('node:fs');
const path = require('node:path');

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
      console.error(`[config] Loaded config from ${label}`);
      return JSON.parse(content);
    } catch {
      if (p === process.env.CONFIG_PATH) {
        console.error(`[config] CONFIG_PATH set but unreadable: ${p}`);
      }
    }
  }
  return null;
}

const fileConfig = loadConfigFile();

const defaults = {
  port: 3010,
  dbPath: path.join(__dirname, 'airgradientz.db'),
  devices: [
    { ip: '192.168.88.6', label: 'outdoor' },
    { ip: '192.168.88.159', label: 'indoor' },
  ],
  pollIntervalMs: 15_000,
  fetchTimeoutMs: 5_000,
  maxApiRows: 10_000,
  shutdownTimeoutMs: 5_000,
};

// Merge: defaults < config file < env vars
const merged = { ...defaults };

if (fileConfig) {
  if (Array.isArray(fileConfig.devices) && fileConfig.devices.length > 0) {
    merged.devices = fileConfig.devices;
  }
  if (typeof fileConfig.pollIntervalMs === 'number' && fileConfig.pollIntervalMs > 0) {
    merged.pollIntervalMs = fileConfig.pollIntervalMs;
  }
  if (typeof fileConfig.fetchTimeoutMs === 'number' && fileConfig.fetchTimeoutMs > 0) {
    merged.fetchTimeoutMs = fileConfig.fetchTimeoutMs;
  }
  if (typeof fileConfig.maxApiRows === 'number' && fileConfig.maxApiRows > 0) {
    merged.maxApiRows = fileConfig.maxApiRows;
  }
}

if (fileConfig?.ports?.nodejs !== undefined && fileConfig?.ports?.nodejs !== null) {
  const port = Number(fileConfig.ports.nodejs);
  if (port > 0 && port <= 65535) {
    merged.port = port;
  }
}

// Env var overrides (highest priority)
if (process.env.PORT) {
  const port = Number(process.env.PORT);
  if (port > 0 && port <= 65535) {
    merged.port = port;
  }
}
if (process.env.DB_PATH) {
  merged.dbPath = process.env.DB_PATH;
}

// Freeze everything
merged.devices = Object.freeze(merged.devices.map(d => Object.freeze(d)));
const config = Object.freeze(merged);

module.exports = config;
