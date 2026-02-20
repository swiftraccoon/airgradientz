'use strict';

const { describe, it, before, after, mock } = require('node:test');
const assert = require('node:assert/strict');
const http = require('node:http');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');

// Temp DB — must be set before requiring modules
const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'ag-http-test-'));
const dbPath = path.join(tmpDir, 'test.db');
process.env.DB_PATH = dbPath;

const db = require('../src/db');

mock.method(console, 'log', () => {});
mock.method(console, 'error', () => {});

const poller = require('../src/poller');
poller.initHealth();

const { handleApi, sendJson } = require('../src/api');

const PUBLIC_DIR = path.join(__dirname, '..', 'public');

// --- MIME types (mirrored from server.js for test reference) ---
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

// --- Helpers (mirrored from server.js) ---
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
  const raw = qIdx === -1 ? url : url.slice(0, qIdx);
  if (raw.length > 1 && raw.endsWith('/')) {
    return raw.slice(0, -1);
  }
  return raw;
}

function serveStatic(res, pathname) {
  const decoded = decodeURIComponent(pathname);
  if (decoded.includes('..')) {
    sendJson(res, 403, { error: 'Forbidden' });
    return;
  }

  const filePath = pathname === '/' ? '/index.html' : pathname;
  const fullPath = path.join(PUBLIC_DIR, filePath);

  const resolved = path.resolve(fullPath);
  if (!resolved.startsWith(PUBLIC_DIR)) {
    sendJson(res, 403, { error: 'Forbidden' });
    return;
  }

  const ext = path.extname(resolved).toLowerCase();
  const contentType = MIME_TYPES[ext] || 'application/octet-stream';

  fs.readFile(resolved, (err, data) => {
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

const startedAt = Date.now();
let requestsServed = 0;
const appLocals = {
  startedAt,
  getRequestsServed: () => requestsServed,
};

let server;
let baseUrl;

before(async () => {
  server = http.createServer((req, res) => {
    requestsServed++;

    // Security headers (mirrored from server.js)
    res.setHeader('X-Content-Type-Options', 'nosniff');
    res.setHeader('X-Frame-Options', 'DENY');
    res.setHeader('Referrer-Policy', 'strict-origin-when-cross-origin');

    // Reject non-GET methods
    if (req.method !== 'GET') {
      sendJson(res, 405, { error: 'Method not allowed' });
      return;
    }

    const pathname = getPathname(req.url);
    const query = parseQuery(req.url);

    // API routes
    if (pathname.startsWith('/api/')) {
      const apiPath = pathname.slice(4);
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
  return res;
}

async function getJson(urlPath) {
  const res = await fetch(`${baseUrl}${urlPath}`);
  const body = await res.json();
  return { status: res.status, headers: res.headers, body };
}

async function request(method, urlPath) {
  const res = await fetch(`${baseUrl}${urlPath}`, { method });
  return res;
}

// Raw HTTP request that bypasses fetch's URL normalization (needed for path traversal tests)
function rawGet(rawPath) {
  return new Promise((resolve, reject) => {
    const url = new URL(baseUrl);
    const reqOpts = {
      hostname: url.hostname,
      port: url.port,
      path: rawPath,
      method: 'GET',
    };
    const req = http.request(reqOpts, (response) => {
      let data = '';
      response.on('data', (chunk) => { data += chunk; });
      response.on('end', () => {
        resolve({ status: response.statusCode, body: JSON.parse(data) });
      });
    });
    req.on('error', reject);
    req.end();
  });
}

// =============================================================================
// handleApi routing
// =============================================================================

describe('handleApi routing', () => {
  it('returns 200 for /api/readings', async () => {
    const { status } = await getJson('/api/readings');
    assert.equal(status, 200);
  });

  it('returns 200 for /api/readings/latest', async () => {
    const { status } = await getJson('/api/readings/latest');
    assert.equal(status, 200);
  });

  it('returns 200 for /api/readings/count', async () => {
    const { status } = await getJson('/api/readings/count');
    assert.equal(status, 200);
  });

  it('returns 200 for /api/devices', async () => {
    const { status } = await getJson('/api/devices');
    assert.equal(status, 200);
  });

  it('returns 200 for /api/health', async () => {
    const { status } = await getJson('/api/health');
    assert.equal(status, 200);
  });

  it('returns 200 for /api/config', async () => {
    const { status } = await getJson('/api/config');
    assert.equal(status, 200);
  });

  it('returns 200 for /api/stats', async () => {
    const { status } = await getJson('/api/stats');
    assert.equal(status, 200);
  });

  it('returns 404 for unknown API path', async () => {
    const { status, body } = await getJson('/api/nonexistent');
    assert.equal(status, 404);
    assert.deepEqual(body, { error: 'Not found' });
  });

  it('returns 404 for /api/readings/invalid', async () => {
    const { status, body } = await getJson('/api/readings/invalid');
    assert.equal(status, 404);
    assert.deepEqual(body, { error: 'Not found' });
  });

  it('returns 404 for bare /api', async () => {
    const { status, body } = await getJson('/api');
    assert.equal(status, 404);
    assert.deepEqual(body, { error: 'Not found' });
  });

  it('returns 404 for /api/ (trailing slash)', async () => {
    const { status } = await getJson('/api/');
    assert.equal(status, 404);
  });
});

// =============================================================================
// JSON response format
// =============================================================================

describe('JSON response format', () => {
  it('sets Content-Type to application/json', async () => {
    const res = await get('/api/health');
    assert.equal(res.headers.get('content-type'), 'application/json');
  });

  it('sets Content-Length header', async () => {
    const res = await get('/api/health');
    const contentLength = res.headers.get('content-length');
    assert.ok(contentLength !== null, 'Content-Length header should be present');
    const body = await res.text();
    assert.equal(Number(contentLength), Buffer.byteLength(body));
  });

  it('error responses have {"error": "message"} shape', async () => {
    const { body } = await getJson('/api/nonexistent');
    assert.ok('error' in body, 'error response should have "error" key');
    assert.equal(typeof body.error, 'string');
  });

  it('404 error body has exactly one key', async () => {
    const { body } = await getJson('/api/nonexistent');
    assert.deepEqual(Object.keys(body), ['error']);
  });

  it('405 error body has {"error": "Method not allowed"}', async () => {
    const res = await request('POST', '/api/health');
    const body = await res.json();
    assert.equal(res.status, 405);
    assert.deepEqual(body, { error: 'Method not allowed' });
  });
});

// =============================================================================
// MIME type detection
// =============================================================================

describe('MIME type detection', () => {
  it('.html serves as text/html', async () => {
    const res = await get('/index.html');
    assert.equal(res.status, 200);
    assert.equal(res.headers.get('content-type'), 'text/html');
  });

  it('.js serves as application/javascript', async () => {
    const res = await get('/app.js');
    assert.equal(res.status, 200);
    assert.equal(res.headers.get('content-type'), 'application/javascript');
  });

  it('.css serves as text/css', async () => {
    const res = await get('/style.css');
    assert.equal(res.status, 200);
    assert.equal(res.headers.get('content-type'), 'text/css');
  });

  it('unknown extension serves as application/octet-stream', async () => {
    // Create a temporary file with unknown extension in public dir
    const testFile = path.join(PUBLIC_DIR, '_test_http_unknown.xyz');
    fs.writeFileSync(testFile, 'test content');
    try {
      const res = await get('/_test_http_unknown.xyz');
      assert.equal(res.status, 200);
      assert.equal(res.headers.get('content-type'), 'application/octet-stream');
    } finally {
      fs.unlinkSync(testFile);
    }
  });
});

// =============================================================================
// Static file serving
// =============================================================================

describe('static file serving', () => {
  it('/ serves index.html', async () => {
    const res = await get('/');
    assert.equal(res.status, 200);
    assert.equal(res.headers.get('content-type'), 'text/html');
    const body = await res.text();
    // Should contain HTML content from the actual index.html
    assert.ok(body.includes('<!DOCTYPE html') || body.includes('<html'),
      'Response should contain HTML markup');
  });

  it('sets Cache-Control header on static files', async () => {
    const res = await get('/index.html');
    assert.equal(res.status, 200);
    assert.equal(res.headers.get('cache-control'), 'public, max-age=600');
  });

  it('sets Content-Length on static files', async () => {
    const res = await get('/index.html');
    const contentLength = res.headers.get('content-length');
    assert.ok(contentLength !== null, 'Content-Length should be present');
    assert.ok(Number(contentLength) > 0, 'Content-Length should be positive');
  });

  it('returns 404 for nonexistent static file', async () => {
    const { status, body } = await getJson('/does-not-exist.html');
    assert.equal(status, 404);
    assert.deepEqual(body, { error: 'Not found' });
  });
});

// =============================================================================
// Path traversal protection
// =============================================================================

describe('path traversal protection', () => {
  it('rejects .. in path with 403', async () => {
    // Use raw http.request because fetch normalizes /../ out of the URL
    const res = await rawGet('/../package.json');
    assert.equal(res.status, 403);
    assert.deepEqual(res.body, { error: 'Forbidden' });
  });

  it('rejects encoded .. (%2e%2e) in path with 403', async () => {
    const res = await rawGet('/%2e%2e/package.json');
    assert.equal(res.status, 403);
    assert.deepEqual(res.body, { error: 'Forbidden' });
  });

  it('rejects mid-path traversal with 403', async () => {
    const res = await rawGet('/subdir/../../../etc/passwd');
    assert.equal(res.status, 403);
    assert.deepEqual(res.body, { error: 'Forbidden' });
  });
});

// =============================================================================
// HTTP method enforcement
// =============================================================================

describe('HTTP method enforcement', () => {
  it('rejects POST with 405', async () => {
    const res = await request('POST', '/api/health');
    assert.equal(res.status, 405);
  });

  it('rejects PUT with 405', async () => {
    const res = await request('PUT', '/api/health');
    assert.equal(res.status, 405);
  });

  it('rejects DELETE with 405', async () => {
    const res = await request('DELETE', '/api/health');
    assert.equal(res.status, 405);
  });

  it('rejects PATCH with 405', async () => {
    const res = await request('PATCH', '/api/health');
    assert.equal(res.status, 405);
  });

  it('405 response has error JSON body', async () => {
    const res = await request('POST', '/');
    const body = await res.json();
    assert.equal(res.status, 405);
    assert.deepEqual(body, { error: 'Method not allowed' });
  });

  it('405 for non-GET on static file paths', async () => {
    const res = await request('POST', '/index.html');
    assert.equal(res.status, 405);
  });
});

// =============================================================================
// Security headers
// =============================================================================

describe('security headers', () => {
  it('X-Content-Type-Options is nosniff on API responses', async () => {
    const res = await get('/api/health');
    assert.equal(res.headers.get('x-content-type-options'), 'nosniff');
  });

  it('X-Frame-Options is DENY on API responses', async () => {
    const res = await get('/api/health');
    assert.equal(res.headers.get('x-frame-options'), 'DENY');
  });

  it('Referrer-Policy is strict-origin-when-cross-origin on API responses', async () => {
    const res = await get('/api/health');
    assert.equal(res.headers.get('referrer-policy'), 'strict-origin-when-cross-origin');
  });

  it('security headers present on static file responses', async () => {
    const res = await get('/index.html');
    assert.equal(res.headers.get('x-content-type-options'), 'nosniff');
    assert.equal(res.headers.get('x-frame-options'), 'DENY');
    assert.equal(res.headers.get('referrer-policy'), 'strict-origin-when-cross-origin');
  });

  it('security headers present on error responses', async () => {
    const res = await get('/api/nonexistent');
    assert.equal(res.headers.get('x-content-type-options'), 'nosniff');
    assert.equal(res.headers.get('x-frame-options'), 'DENY');
    assert.equal(res.headers.get('referrer-policy'), 'strict-origin-when-cross-origin');
  });

  it('security headers present on 405 responses', async () => {
    const res = await request('POST', '/api/health');
    assert.equal(res.headers.get('x-content-type-options'), 'nosniff');
    assert.equal(res.headers.get('x-frame-options'), 'DENY');
    assert.equal(res.headers.get('referrer-policy'), 'strict-origin-when-cross-origin');
  });

  it('does not expose x-powered-by', async () => {
    const res = await get('/api/health');
    assert.equal(res.headers.get('x-powered-by'), null);
  });
});

// =============================================================================
// URL parsing helpers
// =============================================================================

describe('getPathname helper', () => {
  it('returns path without query string', () => {
    assert.equal(getPathname('/api/readings?from=1&to=2'), '/api/readings');
  });

  it('returns path as-is when no query string', () => {
    assert.equal(getPathname('/api/health'), '/api/health');
  });

  it('strips trailing slash', () => {
    assert.equal(getPathname('/api/readings/'), '/api/readings');
  });

  it('preserves root path /', () => {
    assert.equal(getPathname('/'), '/');
  });

  it('strips trailing slash with query string', () => {
    assert.equal(getPathname('/api/readings/?from=1'), '/api/readings');
  });
});

describe('parseQuery helper', () => {
  it('returns empty object for no query string', () => {
    assert.deepEqual(parseQuery('/api/readings'), {});
  });

  it('parses single parameter', () => {
    const q = parseQuery('/api/readings?limit=10');
    assert.equal(q.limit, '10');
  });

  it('parses multiple parameters', () => {
    const q = parseQuery('/api/readings?from=1000&to=2000&device=indoor');
    assert.equal(q.from, '1000');
    assert.equal(q.to, '2000');
    assert.equal(q.device, 'indoor');
  });

  it('handles URL-encoded values', () => {
    const q = parseQuery('/api/readings?device=my%20device');
    assert.equal(q.device, 'my device');
  });

  it('handles empty value', () => {
    const q = parseQuery('/api/readings?device=');
    assert.equal(q.device, '');
  });
});
