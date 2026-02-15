const std = @import("std");
const api = @import("api.zig");
const config_mod = @import("config.zig");
const db_mod = @import("db.zig");
const poller_mod = @import("poller.zig");
const linux = std.os.linux;

const max_request_bytes = 8192;
const max_concurrent_connections = 128;
const max_epoll_events = 64;
const conn_timeout_secs = 30;

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
    poller_state: *poller_mod.PollerState,
    started_at: i64,
    active_connections: std.atomic.Value(u32) = std.atomic.Value(u32).init(0),
    requests_served: std.atomic.Value(u64) = std.atomic.Value(u64).init(0),
    arena_resets: std.atomic.Value(u64) = std.atomic.Value(u64).init(0),
};

const ConnPhase = enum { reading, writing };

const ConnData = struct {
    phase: ConnPhase = .reading,
    accepted_at: i64 = 0,
    read_buf: [max_request_bytes]u8 = [_]u8{0} ** max_request_bytes,
    read_pos: usize = 0,
    response: ?[]u8 = null,
    response_len: usize = 0,
    write_pos: usize = 0,
    arena: std.heap.ArenaAllocator,

    fn init() ConnData {
        return .{
            .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
            .accepted_at = std.time.timestamp(),
        };
    }

    fn deinit(self: *ConnData) void {
        self.arena.deinit();
    }
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

// --- Response builders (return allocated []u8 instead of writing to stream) ---

fn buildResponse(allocator: std.mem.Allocator, status: u16, status_text: []const u8, content_type: []const u8, body: []const u8, extra_headers: ?[]const u8) ![]u8 {
    var hdr: std.ArrayList(u8) = .empty;
    defer hdr.deinit(allocator);

    try std.fmt.format(hdr.writer(allocator), "HTTP/1.1 {d} {s}\r\nConnection: close\r\nX-Content-Type-Options: nosniff\r\nX-Frame-Options: DENY\r\nContent-Type: {s}\r\nContent-Length: {d}\r\n", .{ status, status_text, content_type, body.len });

    if (extra_headers) |eh| {
        try hdr.appendSlice(allocator, eh);
    }
    try hdr.appendSlice(allocator, "\r\n");

    // Combine header and body into a single buffer
    const total_len = hdr.items.len + body.len;
    const result = try allocator.alloc(u8, total_len);
    @memcpy(result[0..hdr.items.len], hdr.items);
    if (body.len > 0) {
        @memcpy(result[hdr.items.len..], body);
    }
    return result;
}

fn buildJsonResponse(allocator: std.mem.Allocator, status: u16, status_text: []const u8, json_val: std.json.Value) ![]u8 {
    const body = try std.json.Stringify.valueAlloc(allocator, json_val, .{});
    defer allocator.free(body);
    return buildResponse(allocator, status, status_text, "application/json", body, null);
}

fn buildError(allocator: std.mem.Allocator, status: u16, status_text: []const u8, msg: []const u8) ![]u8 {
    var body: std.ArrayList(u8) = .empty;
    defer body.deinit(allocator);
    try std.fmt.format(body.writer(allocator), "{{\"error\":\"{s}\"}}", .{msg});
    return buildResponse(allocator, status, status_text, "application/json", body.items, null);
}

fn buildStaticResponse(allocator: std.mem.Allocator, req_path: []const u8) ![]u8 {
    // URL-decode the path first
    var decoded_buf: [512]u8 = [_]u8{0} ** 512;
    const decoded_path = urlDecodePath(&decoded_buf, req_path) orelse {
        return buildError(allocator, 400, "Bad Request", "Bad request");
    };

    // Reject path traversal on decoded path
    if (!isPathSafe(decoded_path)) {
        return buildError(allocator, 403, "Forbidden", "Forbidden");
    }

    // Strip leading slashes
    var relative = decoded_path;
    while (relative.len > 0 and relative[0] == '/') {
        relative = relative[1..];
    }

    // Reject empty segments and control characters
    for (relative) |ch| {
        if (ch < 0x20 or ch == 0x7f) {
            return buildError(allocator, 400, "Bad Request", "Bad request");
        }
    }

    var filepath_buf: [512]u8 = [_]u8{0} ** 512;
    const filepath = if (relative.len == 0)
        std.fmt.bufPrint(&filepath_buf, "public/index.html", .{}) catch {
            return buildError(allocator, 500, "Internal Server Error", "Internal server error");
        }
    else
        std.fmt.bufPrint(&filepath_buf, "public/{s}", .{relative}) catch {
            return buildError(allocator, 414, "URI Too Long", "URI too long");
        };

    const file = std.fs.cwd().openFile(filepath, .{}) catch {
        return buildError(allocator, 404, "Not Found", "Not found");
    };
    defer file.close();

    const stat = file.stat() catch {
        return buildError(allocator, 500, "Internal Server Error", "Internal server error");
    };

    if (stat.kind != .file) {
        return buildError(allocator, 404, "Not Found", "Not found");
    }

    // Cap file size to prevent OOM (16 MB)
    const max_file_size: u64 = 16 * 1024 * 1024;
    if (stat.size > max_file_size) {
        return buildError(allocator, 413, "Payload Too Large", "File too large");
    }

    const file_size: usize = @intCast(stat.size);
    const buf = try allocator.alloc(u8, file_size);
    defer allocator.free(buf);

    const bytes_read = file.readAll(buf) catch {
        return buildError(allocator, 500, "Internal Server Error", "Internal server error");
    };

    const ct = contentTypeFor(filepath);
    return buildResponse(allocator, 200, "OK", ct, buf[0..bytes_read], "Cache-Control: public, max-age=600\r\n");
}

// --- Request parsing from buffer ---

fn parseRequestBuf(data: []const u8) ?HttpRequest {
    if (data.len == 0) return null;

    // Parse: METHOD /path?query HTTP/1.x
    const sp1 = std.mem.indexOfScalar(u8, data, ' ') orelse return null;
    const method = data[0..sp1];

    const uri_start = sp1 + 1;
    if (uri_start >= data.len) return null;
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

// --- Routing: parse request, route, return response buffer ---

fn routeRequest(allocator: std.mem.Allocator, state: *ServerState, data: []const u8) ![]u8 {
    const req = parseRequestBuf(data) orelse {
        return buildError(allocator, 400, "Bad Request", "Bad request");
    };

    _ = state.requests_served.fetchAdd(1, .monotonic);

    if (!std.mem.eql(u8, req.method, "GET")) {
        return buildError(allocator, 405, "Method Not Allowed", "Method not allowed");
    }

    // Route - check /api/readings/latest before /api/readings
    if (std.mem.eql(u8, req.path, "/api/readings/latest")) {
        const result = api.handleReadingsLatest(allocator, state) catch {
            return buildError(allocator, 500, "Internal Server Error", "Internal server error");
        };
        return buildJsonResponse(allocator, 200, "OK", result);
    } else if (std.mem.eql(u8, req.path, "/api/readings")) {
        const result = api.handleReadings(allocator, state, req.query) catch {
            return buildError(allocator, 500, "Internal Server Error", "Internal server error");
        };
        return buildJsonResponse(allocator, 200, "OK", result);
    } else if (std.mem.eql(u8, req.path, "/api/devices")) {
        const result = api.handleDevices(allocator, state) catch {
            return buildError(allocator, 500, "Internal Server Error", "Internal server error");
        };
        return buildJsonResponse(allocator, 200, "OK", result);
    } else if (std.mem.eql(u8, req.path, "/api/health")) {
        const result = api.handleHealth(allocator, state) catch {
            return buildError(allocator, 500, "Internal Server Error", "Internal server error");
        };
        return buildJsonResponse(allocator, 200, "OK", result);
    } else if (std.mem.eql(u8, req.path, "/api/config")) {
        const result = api.handleConfig(allocator, state) catch {
            return buildError(allocator, 500, "Internal Server Error", "Internal server error");
        };
        return buildJsonResponse(allocator, 200, "OK", result);
    } else if (std.mem.eql(u8, req.path, "/api/stats")) {
        const result = api.handleStats(allocator, state) catch {
            return buildError(allocator, 500, "Internal Server Error", "Internal server error");
        };
        return buildJsonResponse(allocator, 200, "OK", result);
    } else {
        return buildStaticResponse(allocator, req.path);
    }
}

// --- Epoll event loop ---

fn setNonBlocking(fd: i32) !void {
    const flags = try std.posix.fcntl(fd, linux.F.GETFL, 0);
    // SOCK.NONBLOCK == O_NONBLOCK (0x800) on Linux x86_64
    _ = try std.posix.fcntl(fd, linux.F.SETFL, flags | @as(usize, linux.SOCK.NONBLOCK));
}

fn closeAndCleanup(epfd: i32, fd: i32, connections: *std.AutoHashMap(i32, *ConnData), state: *ServerState) void {
    // Remove from epoll (ignore errors, fd may already be invalid)
    std.posix.epoll_ctl(epfd, linux.EPOLL.CTL_DEL, fd, null) catch {};

    // Free ConnData — only decrement counter if connection was tracked
    if (connections.fetchRemove(fd)) |kv| {
        var conn = kv.value;
        conn.deinit();
        std.heap.page_allocator.destroy(conn);
        _ = state.active_connections.fetchSub(1, .release);
        _ = state.arena_resets.fetchAdd(1, .monotonic);
    }

    std.posix.close(fd);
}

pub fn run(state: *ServerState) void {
    // Create listen socket manually for epoll control
    const listen_fd = std.posix.socket(std.posix.AF.INET, linux.SOCK.STREAM | linux.SOCK.CLOEXEC, std.posix.IPPROTO.TCP) catch |err| {
        std.log.err("[server] socket() failed: {s}", .{@errorName(err)});
        return;
    };
    defer std.posix.close(listen_fd);

    // Set SO_REUSEADDR
    std.posix.setsockopt(listen_fd, std.posix.SOL.SOCKET, std.posix.SO.REUSEADDR, &std.mem.toBytes(@as(c_int, 1))) catch |err| {
        std.log.err("[server] setsockopt REUSEADDR failed: {s}", .{@errorName(err)});
        return;
    };

    // Bind
    const addr = std.net.Address.parseIp4("0.0.0.0", state.config.port) catch {
        std.log.err("[server] Invalid address", .{});
        return;
    };
    const socklen = addr.getOsSockLen();
    std.posix.bind(listen_fd, &addr.any, socklen) catch |err| {
        std.log.err("[server] bind() failed on port {d}: {s}", .{ state.config.port, @errorName(err) });
        return;
    };

    // Listen
    std.posix.listen(listen_fd, 128) catch |err| {
        std.log.err("[server] listen() failed: {s}", .{@errorName(err)});
        return;
    };

    // Set listen socket non-blocking
    setNonBlocking(listen_fd) catch |err| {
        std.log.err("[server] failed to set listen socket non-blocking: {s}", .{@errorName(err)});
        return;
    };

    // Ignore SIGPIPE so writes to closed sockets return errors instead of killing us
    const pipe_sa = std.posix.Sigaction{
        .handler = .{ .handler = std.posix.SIG.IGN },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    std.posix.sigaction(std.posix.SIG.PIPE, &pipe_sa, null);

    // Create epoll instance
    const epfd = std.posix.epoll_create1(linux.EPOLL.CLOEXEC) catch |err| {
        std.log.err("[server] epoll_create1 failed: {s}", .{@errorName(err)});
        return;
    };
    defer std.posix.close(epfd);

    // Register listen fd with EPOLLIN | EPOLLET
    var listen_ev = linux.epoll_event{
        .events = linux.EPOLL.IN | linux.EPOLL.ET,
        .data = .{ .fd = listen_fd },
    };
    std.posix.epoll_ctl(epfd, linux.EPOLL.CTL_ADD, listen_fd, &listen_ev) catch |err| {
        std.log.err("[server] epoll_ctl ADD listen fd failed: {s}", .{@errorName(err)});
        return;
    };

    // Connection tracking
    var connections = std.AutoHashMap(i32, *ConnData).init(std.heap.page_allocator);
    defer {
        // Clean up any remaining connections
        var it = connections.iterator();
        while (it.next()) |entry| {
            var conn = entry.value_ptr.*;
            conn.deinit();
            std.heap.page_allocator.destroy(conn);
            std.posix.close(entry.key_ptr.*);
        }
        connections.deinit();
    }

    std.log.info("[server] Listening on http://localhost:{d} (epoll)", .{state.config.port});

    var events: [max_epoll_events]linux.epoll_event = undefined;

    // Main event loop
    while (true) {
        if (state.shutdown.load(.acquire)) break;

        // 1-second timeout for shutdown check
        const nfds = std.posix.epoll_wait(epfd, &events, 1000);

        if (state.shutdown.load(.acquire)) break;

        for (events[0..nfds]) |ev| {
            if (ev.data.fd == listen_fd) {
                // Accept loop (edge-triggered: must drain all pending)
                while (true) {
                    const client_fd = std.posix.accept(listen_fd, null, null, linux.SOCK.NONBLOCK | linux.SOCK.CLOEXEC) catch |err| {
                        if (err == error.WouldBlock) break;
                        if (err == error.ConnectionAborted) continue;
                        std.log.err("[server] accept error: {s}", .{@errorName(err)});
                        break;
                    };

                    // Connection limit check
                    const current = state.active_connections.fetchAdd(1, .acquire);
                    if (current >= max_concurrent_connections) {
                        _ = state.active_connections.fetchSub(1, .release);
                        std.log.warn("[server] connection limit reached ({d}), rejecting", .{max_concurrent_connections});
                        std.posix.close(client_fd);
                        continue;
                    }

                    // Allocate ConnData
                    const conn = std.heap.page_allocator.create(ConnData) catch {
                        _ = state.active_connections.fetchSub(1, .release);
                        std.log.err("[server] failed to allocate ConnData", .{});
                        std.posix.close(client_fd);
                        continue;
                    };
                    conn.* = ConnData.init();

                    connections.put(client_fd, conn) catch {
                        conn.deinit();
                        std.heap.page_allocator.destroy(conn);
                        _ = state.active_connections.fetchSub(1, .release);
                        std.log.err("[server] failed to track connection", .{});
                        std.posix.close(client_fd);
                        continue;
                    };

                    // Register with EPOLLIN | EPOLLET
                    var client_ev = linux.epoll_event{
                        .events = linux.EPOLL.IN | linux.EPOLL.ET,
                        .data = .{ .fd = client_fd },
                    };
                    std.posix.epoll_ctl(epfd, linux.EPOLL.CTL_ADD, client_fd, &client_ev) catch |err| {
                        std.log.err("[server] epoll_ctl ADD client fd failed: {s}", .{@errorName(err)});
                        closeAndCleanup(epfd, client_fd, &connections, state);
                        continue;
                    };
                }
            } else {
                // Client fd event
                const fd = ev.data.fd;
                const conn = connections.get(fd) orelse {
                    // Stale event — fd already cleaned up
                    continue;
                };

                if (ev.events & (linux.EPOLL.ERR | linux.EPOLL.HUP) != 0) {
                    closeAndCleanup(epfd, fd, &connections, state);
                    continue;
                }

                if (conn.phase == .reading and (ev.events & linux.EPOLL.IN != 0)) {
                    // Read loop (edge-triggered: drain until WouldBlock)
                    var should_close = false;
                    var request_ready = false;

                    while (conn.read_pos < conn.read_buf.len) {
                        const n = std.posix.read(fd, conn.read_buf[conn.read_pos..]) catch |err| {
                            if (err == error.WouldBlock) break;
                            std.log.debug("[server] read error on fd {d}: {s}", .{ fd, @errorName(err) });
                            should_close = true;
                            break;
                        };
                        if (n == 0) {
                            // Client closed connection
                            should_close = true;
                            break;
                        }
                        conn.read_pos += n;

                        // Check if we have a complete HTTP request (look for \r\n\r\n)
                        if (std.mem.indexOf(u8, conn.read_buf[0..conn.read_pos], "\r\n\r\n")) |_| {
                            request_ready = true;
                            break;
                        }
                    }

                    // Buffer full without finding end of headers — also treat as ready
                    if (conn.read_pos >= conn.read_buf.len and !request_ready) {
                        request_ready = true;
                    }

                    if (should_close) {
                        closeAndCleanup(epfd, fd, &connections, state);
                        continue;
                    }

                    if (request_ready) {
                        const allocator = conn.arena.allocator();
                        const response = routeRequest(allocator, state, conn.read_buf[0..conn.read_pos]) catch |err| {
                            std.log.err("[server] route error: {s}", .{@errorName(err)});
                            closeAndCleanup(epfd, fd, &connections, state);
                            continue;
                        };

                        conn.response = response;
                        conn.response_len = response.len;
                        conn.write_pos = 0;
                        conn.phase = .writing;

                        // Switch to EPOLLOUT | EPOLLET
                        var mod_ev = linux.epoll_event{
                            .events = linux.EPOLL.OUT | linux.EPOLL.ET,
                            .data = .{ .fd = fd },
                        };
                        std.posix.epoll_ctl(epfd, linux.EPOLL.CTL_MOD, fd, &mod_ev) catch |err| {
                            std.log.err("[server] epoll_ctl MOD to EPOLLOUT failed: {s}", .{@errorName(err)});
                            closeAndCleanup(epfd, fd, &connections, state);
                            continue;
                        };
                    }
                } else if (conn.phase == .writing and (ev.events & linux.EPOLL.OUT != 0)) {
                    // Write loop (edge-triggered: write until WouldBlock or done)
                    const resp = conn.response orelse {
                        closeAndCleanup(epfd, fd, &connections, state);
                        continue;
                    };

                    var should_close = false;
                    while (conn.write_pos < conn.response_len) {
                        const n = std.posix.write(fd, resp[conn.write_pos..conn.response_len]) catch |err| {
                            if (err == error.WouldBlock) break;
                            std.log.debug("[server] write error on fd {d}: {s}", .{ fd, @errorName(err) });
                            should_close = true;
                            break;
                        };
                        if (n == 0) {
                            should_close = true;
                            break;
                        }
                        conn.write_pos += n;
                    }

                    if (should_close or conn.write_pos >= conn.response_len) {
                        // Done writing (or error) — close connection
                        closeAndCleanup(epfd, fd, &connections, state);
                    }
                }
            }
        }

        // Sweep stale connections (Slowloris defense)
        const now = std.time.timestamp();
        var stale_fds: [max_concurrent_connections]i32 = undefined;
        var stale_count: usize = 0;
        var sweep_it = connections.iterator();
        while (sweep_it.next()) |entry| {
            if (now - entry.value_ptr.*.accepted_at > conn_timeout_secs) {
                if (stale_count < stale_fds.len) {
                    stale_fds[stale_count] = entry.key_ptr.*;
                    stale_count += 1;
                }
            }
        }
        for (stale_fds[0..stale_count]) |stale_fd| {
            closeAndCleanup(epfd, stale_fd, &connections, state);
        }
    }

    std.log.info("[server] Event loop exiting", .{});
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
