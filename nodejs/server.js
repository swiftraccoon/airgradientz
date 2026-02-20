'use strict';

const http = require('node:http');
const fs = require('node:fs');
const path = require('node:path');
const config = require('./config');
const { timestamp } = require('./src/log');
const { handleApi, sendJson } = require('./src/api');
const { startPoller, stopPoller } = require('./src/poller');
const { close: closeDb } = require('./src/db');

// --- Version gate ---
const [major] = process.versions.node.split('.').map(Number);
if (major < 24) {
  console.error(`[server] Node.js >= 24 required (running ${process.version})`);
  process.exit(1);
}

// --- State ---
const startedAt = Date.now();
let requestsServed = 0;

const appLocals = {
  startedAt,
  getRequestsServed: () => requestsServed,
};

// --- MIME types for static file serving ---
const MIME_TYPES = Object.freeze({
  '.html': 'text/html',
  '.css': 'text/css',
  '.js': 'application/javascript',
  '.json': 'application/json',
  '.png': 'image/png',
  '.jpg': 'image/jpeg',
  '.jpeg': 'image/jpeg',
  '.gif': 'image/gif',
  '.svg': 'image/svg+xml',
  '.ico': 'image/x-icon',
  '.woff': 'font/woff',
  '.woff2': 'font/woff2',
  '.ttf': 'font/ttf',
  '.txt': 'text/plain',
});

const PUBLIC_DIR = path.join(__dirname, 'public');

function parseQuery(url) {
  const params = {};
  const qIdx = url.indexOf('?');
  if (qIdx === -1) {
    return params;
  }
  const searchParams = new URLSearchParams(url.slice(qIdx + 1));
  for (const [key, value] of searchParams) {
    params[key] = value; // eslint-disable-line security/detect-object-injection
  }
  return params;
}

function getPathname(url) {
  const qIdx = url.indexOf('?');
  const raw = qIdx === -1 ? url : url.slice(0, qIdx);
  // Normalize: remove trailing slash (except for root)
  if (raw.length > 1 && raw.endsWith('/')) {
    return raw.slice(0, -1);
  }
  return raw;
}

function serveStatic(res, pathname) {
  // Path traversal detection (check decoded path)
  const decoded = decodeURIComponent(pathname);
  if (decoded.includes('..')) {
    sendJson(res, 403, { error: 'Forbidden' });
    return;
  }

  // Map / to /index.html
  const filePath = pathname === '/' ? '/index.html' : pathname;
  const fullPath = path.join(PUBLIC_DIR, filePath);

  // Double-check resolved path is within PUBLIC_DIR
  const resolved = path.resolve(fullPath);
  if (!resolved.startsWith(PUBLIC_DIR)) {
    sendJson(res, 403, { error: 'Forbidden' });
    return;
  }

  // Determine MIME type
  const ext = path.extname(resolved).toLowerCase();
  // eslint-disable-next-line security/detect-object-injection
  const contentType = MIME_TYPES[ext] || 'application/octet-stream';

  // Read and serve file
  // resolved comes from joining PUBLIC_DIR (hardcoded) with the URL path after traversal checks.
  // Not user-controlled beyond the validated pathname.
  fs.readFile(resolved, (err, data) => { // eslint-disable-line security/detect-non-literal-fs-filename
    if (err) {
      sendJson(res, 404, { error: 'Not found' });
      return;
    }
    res.writeHead(200, {
      'Content-Type': contentType,
      'Content-Length': data.length,
      'Cache-Control': 'public, max-age=600',
    });
    res.end(data);
  });
}

// --- HTTP Server ---
const server = http.createServer((req, res) => {
  requestsServed++;

  // Security headers (on every response)
  res.setHeader('X-Content-Type-Options', 'nosniff');
  res.setHeader('X-Frame-Options', 'DENY');
  res.setHeader('Referrer-Policy', 'strict-origin-when-cross-origin');

  // Reject non-GET methods (405 Method Not Allowed)
  if (req.method !== 'GET') {
    sendJson(res, 405, { error: 'Method not allowed' });
    return;
  }

  const pathname = getPathname(req.url);
  const query = parseQuery(req.url);

  // API routes
  if (pathname.startsWith('/api/')) {
    const apiPath = pathname.slice(4); // strip "/api" prefix -> "/readings", "/health", etc.
    const handled = handleApi(req, res, apiPath, query, appLocals);
    if (!handled) {
      sendJson(res, 404, { error: 'Not found' });
    }
    return;
  }
  if (pathname === '/api') {
    sendJson(res, 404, { error: 'Not found' });
    return;
  }

  // Static file serving
  serveStatic(res, pathname);
});

// --- Start ---
server.listen(config.port, () => {
  console.log(`${timestamp()} [server] Listening on http://localhost:${config.port} (Node ${process.version})`);
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
  if (shuttingDown) {
    return;
  }
  shuttingDown = true;
  console.log(`${timestamp()} [server] ${signal} received, shutting down`);
  stopPoller();
  server.close(() => {
    closeDb();
    console.log(`${timestamp()} [server] Shutdown complete`);
    process.exit(0);
  });
  setTimeout(() => {
    console.error(`${timestamp()} [server] Forced exit after timeout`);
    process.exit(1);
  }, config.shutdownTimeoutMs);
}

process.on('SIGTERM', () => shutdown('SIGTERM'));
process.on('SIGINT', () => shutdown('SIGINT'));

// --- Process hardening ---
process.on('uncaughtException', (err) => {
  console.error(`${timestamp()} [server] Uncaught exception:`, err);
  shutdown('uncaughtException');
});

process.on('unhandledRejection', (reason) => {
  console.error(`${timestamp()} [server] Unhandled rejection:`, reason);
  shutdown('unhandledRejection');
});
