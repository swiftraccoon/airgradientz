'use strict';

const path = require('node:path');

const config = Object.freeze({
  port: Number(process.env.PORT) || 3010,
  dbPath: process.env.DB_PATH || path.join(__dirname, 'airgradientz.db'),

  devices: Object.freeze([
    Object.freeze({ ip: '192.168.88.6', label: 'outdoor' }),
    Object.freeze({ ip: '192.168.88.159', label: 'indoor' }),
  ]),
  pollIntervalMs: 15_000,
  fetchTimeoutMs: 5_000,

  maxApiRows: 10_000,
  shutdownTimeoutMs: 5_000,
});

module.exports = config;
