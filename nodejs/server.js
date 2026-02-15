'use strict';

const express = require('express');
const path = require('node:path');
const config = require('./config');
const apiRouter = require('./src/api');
const { startPoller, stopPoller } = require('./src/poller');
const { close: closeDb } = require('./src/db');

// --- Version gate ---
const [major] = process.versions.node.split('.').map(Number);
if (major < 24) {
  console.error(`[server] Node.js >= 24 required (running ${process.version})`);
  process.exit(1);
}

// --- Express setup ---
const startedAt = Date.now();
let requestsServed = 0;

const app = express();

// Security: remove fingerprint header
app.disable('x-powered-by');

// Security headers
app.use((_req, res, next) => {
  requestsServed++;
  res.setHeader('X-Content-Type-Options', 'nosniff');
  res.setHeader('X-Frame-Options', 'DENY');
  res.setHeader('Referrer-Policy', 'strict-origin-when-cross-origin');
  next();
});

app.locals.startedAt = startedAt;
app.locals.getRequestsServed = () => requestsServed;

app.use('/api', apiRouter);
app.use(express.static(path.join(__dirname, 'public'), {
  maxAge: '10m',
  etag: true,
}));

// --- Start ---
const server = app.listen(config.port, () => {
  console.log(`[server] Listening on http://localhost:${config.port} (Node ${process.version})`);
  startPoller();
});

server.on('error', (err) => {
  if (err.code === 'EADDRINUSE') {
    console.error(`[server] Port ${config.port} is already in use`);
  } else {
    console.error(`[server] Failed to start: ${err.message}`);
  }
  process.exit(1);
});

// --- Graceful shutdown ---
let shuttingDown = false;

function shutdown(signal) {
  if (shuttingDown) return;
  shuttingDown = true;
  console.log(`[server] ${signal} received, shutting down`);
  stopPoller();
  server.close(() => {
    closeDb();
    console.log('[server] Shutdown complete');
    process.exit(0);
  });
  setTimeout(() => {
    console.error('[server] Forced exit after timeout');
    process.exit(1);
  }, config.shutdownTimeoutMs);
}

process.on('SIGTERM', () => shutdown('SIGTERM'));
process.on('SIGINT', () => shutdown('SIGINT'));

// --- Process hardening ---
process.on('uncaughtException', (err) => {
  console.error('[server] Uncaught exception:', err);
  shutdown('uncaughtException');
});

process.on('unhandledRejection', (reason) => {
  console.error('[server] Unhandled rejection:', reason);
  shutdown('unhandledRejection');
});
