const std = @import("std");
const api = @import("api.zig");
const config_mod = @import("config.zig");
const db_mod = @import("db.zig");
const poller_mod = @import("poller.zig");

const max_request_bytes = 8192;
const max_concurrent_connections = 128;

pub const HttpRequest = struct {
    method: []const u8,
    path: []const u8,
    query: []const u8,
};

pub const ServerState = struct {
    db: db_mod.SqliteDb,
    db_mutex: *std.Thread.Mutex,
    health: []poller_mod.DeviceHealth,
    health_mutex: *std.Thread.Mutex,
    config: *const config_mod.Config,
    shutdown: *std.atomic.Value(bool),
    active_connections: std.atomic.Value(u32) = std.atomic.Value(u32).init(0),
};

fn contentTypeFor(path: []const u8) []const u8 {
    if (std.mem.endsWith(u8, path, ".html")) return "text/html; charset=utf-8";
    if (std.mem.endsWith(u8, path, ".css")) return "text/css; charset=utf-8";
    if (std.mem.endsWith(u8, path, ".js")) return "application/javascript; charset=utf-8";
    if (std.mem.endsWith(u8, path, ".json")) return "application/json; charset=utf-8";
    if (std.mem.endsWith(u8, path, ".png")) return "image/png";
    if (std.mem.endsWith(u8, path, ".jpg") or std.mem.endsWith(u8, path, ".jpeg")) return "image/jpeg";
    if (std.mem.endsWith(u8, path, ".svg")) return "image/svg+xml";
    if (std.mem.endsWith(u8, path, ".ico")) return "image/x-icon";
    return "application/octet-stream";
}

fn writeAll(stream: std.net.Stream, data: []const u8) void {
    var sent: usize = 0;
    while (sent < data.len) {
        const n = std.posix.write(stream.handle, data[sent..]) catch |err| {
            std.log.debug("[server] write error: {s}", .{@errorName(err)});
            return;
        };
        if (n == 0) return;
        sent += n;
    }
}

fn sendResponse(stream: std.net.Stream, allocator: std.mem.Allocator, status: u16, status_text: []const u8, content_type: []const u8, body: []const u8, extra_headers: ?[]const u8) void {
    var hdr: std.ArrayList(u8) = .empty;
    defer hdr.deinit(allocator);

    std.fmt.format(hdr.writer(allocator), "HTTP/1.1 {d} {s}\r\nConnection: close\r\nX-Content-Type-Options: nosniff\r\nX-Frame-Options: DENY\r\nContent-Type: {s}\r\nContent-Length: {d}\r\n", .{ status, status_text, content_type, body.len }) catch |err| {
        std.log.err("[server] failed to format response header: {s}", .{@errorName(err)});
        return;
    };

    if (extra_headers) |eh| {
        hdr.appendSlice(allocator, eh) catch return;
    }
    hdr.appendSlice(allocator, "\r\n") catch return;

    writeAll(stream, hdr.items);
    if (body.len > 0) {
        writeAll(stream, body);
    }
}

fn sendJsonResponse(stream: std.net.Stream, allocator: std.mem.Allocator, status: u16, status_text: []const u8, json_val: std.json.Value) void {
    const body = std.json.Stringify.valueAlloc(allocator, json_val, .{}) catch |err| {
        std.log.err("[server] JSON serialization failed: {s}", .{@errorName(err)});
        return;
    };
    defer allocator.free(body);
    sendResponse(stream, allocator, status, status_text, "application/json", body, null);
}

fn sendError(stream: std.net.Stream, allocator: std.mem.Allocator, status: u16, status_text: []const u8, msg: []const u8) void {
    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(allocator);
    std.fmt.format(body.writer(allocator), "{{\"error\":\"{s}\"}}", .{msg}) catch return;
    sendResponse(stream, allocator, status, status_text, "application/json", body.items, null);
}

// URL-decode a path in-place into a buffer. Rejects encoded null bytes.
fn urlDecodePath(dest: []u8, src: []const u8) ?[]const u8 {
    var j: usize = 0;
    var i: usize = 0;
    while (i < src.len) : (i += 1) {
        if (j >= dest.len) return null; // too long
        if (src[i] == '%' and i + 2 < src.len) {
            const hi = hexVal(src[i + 1]) orelse {
                dest[j] = src[i];
                j += 1;
                continue;
            };
            const lo = hexVal(src[i + 2]) orelse {
                dest[j] = src[i];
                j += 1;
                continue;
            };
            const byte = (hi << 4) | lo;
            if (byte == 0) return null; // reject null bytes
            dest[j] = byte;
            j += 1;
            i += 2;
        } else {
            dest[j] = src[i];
            j += 1;
        }
    }
    return dest[0..j];
}

fn hexVal(ch: u8) ?u8 {
    if (ch >= '0' and ch <= '9') return ch - '0';
    if (ch >= 'a' and ch <= 'f') return ch - 'a' + 10;
    if (ch >= 'A' and ch <= 'F') return ch - 'A' + 10;
    return null;
}

// Validate that a path is safe: no ".." components after URL decoding
fn isPathSafe(path: []const u8) bool {
    // Reject any path containing ".."
    var remaining = path;
    while (remaining.len > 0) {
        if (std.mem.startsWith(u8, remaining, "..")) {
            // ".." at start, or after "/" is traversal
            if (remaining.len == 2 or remaining[2] == '/') return false;
        }
        // Advance past next "/"
        if (std.mem.indexOfScalar(u8, remaining, '/')) |slash| {
            remaining = remaining[slash + 1 ..];
        } else {
            break;
        }
    }
    return true;
}

fn serveStatic(stream: std.net.Stream, allocator: std.mem.Allocator, req_path: []const u8) void {
    // URL-decode the path first
    var decoded_buf: [512]u8 = [_]u8{0} ** 512;
    const decoded_path = urlDecodePath(&decoded_buf, req_path) orelse {
        sendError(stream, allocator, 400, "Bad Request", "Bad request");
        return;
    };

    // Reject path traversal on decoded path
    if (!isPathSafe(decoded_path)) {
        sendError(stream, allocator, 403, "Forbidden", "Forbidden");
        return;
    }

    // Strip leading slashes
    var relative = decoded_path;
    while (relative.len > 0 and relative[0] == '/') {
        relative = relative[1..];
    }

    // Reject empty segments and control characters
    for (relative) |ch| {
        if (ch < 0x20 or ch == 0x7f) {
            sendError(stream, allocator, 400, "Bad Request", "Bad request");
            return;
        }
    }

    var filepath_buf: [512]u8 = [_]u8{0} ** 512;
    const filepath = if (relative.len == 0)
        std.fmt.bufPrint(&filepath_buf, "public/index.html", .{}) catch {
            sendError(stream, allocator, 500, "Internal Server Error", "Internal server error");
            return;
        }
    else
        std.fmt.bufPrint(&filepath_buf, "public/{s}", .{relative}) catch {
            sendError(stream, allocator, 414, "URI Too Long", "URI too long");
            return;
        };

    const file = std.fs.cwd().openFile(filepath, .{}) catch {
        sendError(stream, allocator, 404, "Not Found", "Not found");
        return;
    };
    defer file.close();

    const stat = file.stat() catch {
        sendError(stream, allocator, 500, "Internal Server Error", "Internal server error");
        return;
    };

    if (stat.kind != .file) {
        sendError(stream, allocator, 404, "Not Found", "Not found");
        return;
    }

    // Cap file size to prevent OOM (16 MB)
    const max_file_size: u64 = 16 * 1024 * 1024;
    if (stat.size > max_file_size) {
        sendError(stream, allocator, 413, "Payload Too Large", "File too large");
        return;
    }

    const file_size: usize = @intCast(stat.size);
    const buf = allocator.alloc(u8, file_size) catch {
        sendError(stream, allocator, 500, "Internal Server Error", "Internal server error");
        return;
    };
    defer allocator.free(buf);

    const bytes_read = file.readAll(buf) catch {
        sendError(stream, allocator, 500, "Internal Server Error", "Internal server error");
        return;
    };

    const ct = contentTypeFor(filepath);
    sendResponse(stream, allocator, 200, "OK", ct, buf[0..bytes_read], "Cache-Control: public, max-age=600\r\n");
}

fn parseRequest(stream: std.net.Stream, buf: []u8) ?HttpRequest {
    // Set read timeout
    const tv = std.posix.timeval{ .sec = 10, .usec = 0 };
    std.posix.setsockopt(stream.handle, std.posix.SOL.SOCKET, std.posix.SO.RCVTIMEO, std.mem.asBytes(&tv)) catch |err| {
        std.log.warn("[server] failed to set recv timeout: {s}", .{@errorName(err)});
    };

    const n = std.posix.read(stream.handle, buf) catch |err| {
        std.log.debug("[server] request read error: {s}", .{@errorName(err)});
        return null;
    };
    if (n == 0) return null;
    const data = buf[0..n];

    // Parse: METHOD /path?query HTTP/1.x
    const sp1 = std.mem.indexOfScalar(u8, data, ' ') orelse return null;
    const method = data[0..sp1];

    const uri_start = sp1 + 1;
    const rest = data[uri_start..];
    const sp2 = std.mem.indexOfScalar(u8, rest, ' ') orelse return null;
    const uri = rest[0..sp2];

    // Reject unreasonably long URIs
    if (uri.len > 2048) return null;

    // Split path and query
    if (std.mem.indexOfScalar(u8, uri, '?')) |qmark| {
        return HttpRequest{
            .method = method,
            .path = uri[0..qmark],
            .query = uri[qmark + 1 ..],
        };
    }

    return HttpRequest{
        .method = method,
        .path = uri,
        .query = "",
    };
}

fn handleConnection(stream: std.net.Stream, state: *ServerState) void {
    defer {
        stream.close();
        _ = state.active_connections.fetchSub(1, .release);
    }

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var buf: [max_request_bytes]u8 = [_]u8{0} ** max_request_bytes;
    const req = parseRequest(stream, &buf) orelse return;

    if (!std.mem.eql(u8, req.method, "GET")) {
        sendError(stream, allocator, 405, "Method Not Allowed", "Method not allowed");
        return;
    }

    // Route - check /api/readings/latest before /api/readings
    if (std.mem.eql(u8, req.path, "/api/readings/latest")) {
        const result = api.handleReadingsLatest(allocator, state) catch {
            sendError(stream, allocator, 500, "Internal Server Error", "Internal server error");
            return;
        };
        sendJsonResponse(stream, allocator, 200, "OK", result);
    } else if (std.mem.eql(u8, req.path, "/api/readings")) {
        const result = api.handleReadings(allocator, state, req.query) catch {
            sendError(stream, allocator, 500, "Internal Server Error", "Internal server error");
            return;
        };
        sendJsonResponse(stream, allocator, 200, "OK", result);
    } else if (std.mem.eql(u8, req.path, "/api/devices")) {
        const result = api.handleDevices(allocator, state) catch {
            sendError(stream, allocator, 500, "Internal Server Error", "Internal server error");
            return;
        };
        sendJsonResponse(stream, allocator, 200, "OK", result);
    } else if (std.mem.eql(u8, req.path, "/api/health")) {
        const result = api.handleHealth(allocator, state) catch {
            sendError(stream, allocator, 500, "Internal Server Error", "Internal server error");
            return;
        };
        sendJsonResponse(stream, allocator, 200, "OK", result);
    } else if (std.mem.eql(u8, req.path, "/api/config")) {
        const result = api.handleConfig(allocator, state) catch {
            sendError(stream, allocator, 500, "Internal Server Error", "Internal server error");
            return;
        };
        sendJsonResponse(stream, allocator, 200, "OK", result);
    } else {
        serveStatic(stream, allocator, req.path);
    }
}

pub fn run(state: *ServerState) void {
    const addr = std.net.Address.parseIp4("0.0.0.0", state.config.port) catch {
        std.log.err("[server] Invalid address", .{});
        return;
    };

    var server = addr.listen(.{
        .reuse_address = true,
    }) catch {
        std.log.err("[server] Failed to bind port {d}", .{state.config.port});
        return;
    };
    defer server.deinit();

    std.log.info("[server] Listening on http://localhost:{d}", .{state.config.port});

    while (true) {
        if (state.shutdown.load(.acquire)) break;

        const conn = server.accept() catch |err| {
            if (err == error.ConnectionAborted) continue;
            std.log.err("[server] accept error: {s}", .{@errorName(err)});
            continue;
        };

        // Connection limit
        const current = state.active_connections.fetchAdd(1, .acquire);
        if (current >= max_concurrent_connections) {
            _ = state.active_connections.fetchSub(1, .release);
            std.log.warn("[server] connection limit reached ({d}), rejecting", .{max_concurrent_connections});
            conn.stream.close();
            continue;
        }

        const thread = std.Thread.spawn(.{}, handleConnection, .{ conn.stream, state }) catch |err| {
            _ = state.active_connections.fetchSub(1, .release);
            std.log.err("[server] thread spawn failed: {s}", .{@errorName(err)});
            conn.stream.close();
            continue;
        };
        thread.detach();
    }
}

// Query parameter helpers
pub fn queryParam(query: []const u8, name: []const u8) ?[]const u8 {
    if (query.len == 0) return null;

    var remaining = query;
    while (remaining.len > 0) {
        const amp = std.mem.indexOfScalar(u8, remaining, '&');
        const pair = if (amp) |a| remaining[0..a] else remaining;

        if (std.mem.indexOfScalar(u8, pair, '=')) |eq| {
            const key = pair[0..eq];
            if (std.mem.eql(u8, key, name)) {
                return pair[eq + 1 ..];
            }
        } else {
            if (std.mem.eql(u8, pair, name)) {
                return "";
            }
        }

        if (amp) |a| {
            remaining = remaining[a + 1 ..];
        } else {
            break;
        }
    }

    return null;
}

pub fn parseI64Param(query: []const u8, name: []const u8, default: i64) i64 {
    const val = queryParam(query, name) orelse return default;
    return std.fmt.parseInt(i64, val, 10) catch default;
}
