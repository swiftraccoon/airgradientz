const std = @import("std");

pub const HttpGetError = error{
    ConnectionClosed,
    Timeout,
    ConnectionRefused,
    NetworkError,
    RequestTooLong,
    InvalidResponse,
    HttpError,
    OutOfMemory,
};

pub fn httpGet(allocator: std.mem.Allocator, ip: []const u8, path: []const u8, timeout_ms: u32) HttpGetError![]const u8 {
    var hostname: []const u8 = ip;
    var port: u16 = 80;
    if (std.mem.indexOfScalar(u8, ip, ':')) |colon_pos| {
        hostname = ip[0..colon_pos];
        port = std.fmt.parseInt(u16, ip[colon_pos + 1 ..], 10) catch return error.NetworkError;
    }
    const addr = std.net.Address.parseIp4(hostname, port) catch return error.NetworkError;

    const stream = std.net.tcpConnectToAddress(addr) catch |err| {
        return switch (err) {
            error.ConnectionTimedOut => error.Timeout,
            error.ConnectionRefused => error.ConnectionRefused,
            else => error.NetworkError,
        };
    };
    defer stream.close();

    // Set timeouts — log if this fails (connection will work but may hang)
    const tv = std.posix.timeval{
        .sec = @intCast(timeout_ms / 1000),
        .usec = @intCast((timeout_ms % 1000) * 1000),
    };
    std.posix.setsockopt(stream.handle, std.posix.SOL.SOCKET, std.posix.SO.SNDTIMEO, std.mem.asBytes(&tv)) catch |err| {
        std.log.warn("[http_client] failed to set send timeout: {s}", .{@errorName(err)});
    };
    std.posix.setsockopt(stream.handle, std.posix.SOL.SOCKET, std.posix.SO.RCVTIMEO, std.mem.asBytes(&tv)) catch |err| {
        std.log.warn("[http_client] failed to set recv timeout: {s}", .{@errorName(err)});
    };

    // Build and send request
    var req_buf: [512]u8 = [_]u8{0} ** 512;
    const req = std.fmt.bufPrint(&req_buf, "GET {s} HTTP/1.1\r\nHost: {s}\r\nConnection: close\r\n\r\n", .{ path, hostname }) catch return error.RequestTooLong;

    var sent: usize = 0;
    while (sent < req.len) {
        const n = std.posix.write(stream.handle, req[sent..]) catch return error.NetworkError;
        if (n == 0) return error.ConnectionClosed;
        sent += n;
    }

    // Read response
    var response: std.ArrayList(u8) = .empty;
    defer response.deinit(allocator);

    // Cap response size to prevent OOM from malicious server (1 MB)
    const max_response_size: usize = 1024 * 1024;
    var buf: [4096]u8 = [_]u8{0} ** 4096;
    while (true) {
        const n = std.posix.read(stream.handle, &buf) catch return error.NetworkError;
        if (n == 0) break;
        if (response.items.len + n > max_response_size) return error.InvalidResponse;
        response.appendSlice(allocator, buf[0..n]) catch return error.OutOfMemory;
    }

    const data = response.items;
    if (data.len == 0) return error.InvalidResponse;

    // Parse status line
    const eol = std.mem.indexOf(u8, data, "\r\n") orelse return error.InvalidResponse;
    const status_line = data[0..eol];

    const sp = std.mem.indexOfScalar(u8, status_line, ' ') orelse return error.InvalidResponse;
    const status_str = status_line[sp + 1 ..];
    if (status_str.len < 3) return error.InvalidResponse;
    const status_code = std.fmt.parseInt(u16, status_str[0..3], 10) catch return error.InvalidResponse;
    if (status_code < 200 or status_code >= 300) return error.HttpError;

    // Find body after \r\n\r\n
    const header_end = std.mem.indexOf(u8, data, "\r\n\r\n") orelse return error.InvalidResponse;
    const body = data[header_end + 4 ..];

    return allocator.dupe(u8, body) catch return error.OutOfMemory;
}
