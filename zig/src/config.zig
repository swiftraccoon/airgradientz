const std = @import("std");

pub const max_devices = 8;
pub const max_downsample_buckets = 16;

pub const DeviceConfig = struct {
    ip: []const u8,
    label: []const u8,
};

pub const Config = struct {
    port: u16,
    db_path: []const u8,
    devices: [max_devices]DeviceConfig,
    device_count: usize,
    poll_interval_ms: u32,
    fetch_timeout_ms: u32,
    max_api_rows: u32,
    downsample_bucket_count: usize,
    downsample_keys: [max_downsample_buckets][]const u8,
    downsample_values: [max_downsample_buckets]i64,
};

const empty_device = DeviceConfig{ .ip = "", .label = "" };

pub fn fromEnv() Config {
    const allocator = std.heap.page_allocator;

    // Config file is mandatory
    const content = findAndReadConfigFile(allocator) orelse {
        std.log.err("[config] FATAL: no config file found (searched CONFIG_PATH, ./airgradientz.json, ../airgradientz.json)", .{});
        std.process.exit(1);
    };

    const parsed = std.json.parseFromSlice(std.json.Value, allocator, content, .{}) catch |e| {
        std.log.err("[config] FATAL: JSON parse error: {}", .{e});
        std.process.exit(1);
    };

    const root = parsed.value;
    if (root != .object) {
        std.log.err("[config] FATAL: config file root is not a JSON object", .{});
        std.process.exit(1);
    }

    var config = Config{
        .port = 0,
        .db_path = "./airgradientz.db",
        .devices = [_]DeviceConfig{empty_device} ** max_devices,
        .device_count = 0,
        .poll_interval_ms = 0,
        .fetch_timeout_ms = 0,
        .max_api_rows = 0,
        .downsample_bucket_count = 0,
        .downsample_keys = [_][]const u8{""} ** max_downsample_buckets,
        .downsample_values = [_]i64{0} ** max_downsample_buckets,
    };

    // Read devices
    if (root.object.get("devices")) |devices_val| {
        if (devices_val == .array) {
            var count: usize = 0;
            for (devices_val.array.items) |dev| {
                if (count >= max_devices) break;
                if (dev != .object) continue;
                const ip_val = dev.object.get("ip") orelse continue;
                const label_val = dev.object.get("label") orelse continue;
                if (ip_val != .string or label_val != .string) continue;
                config.devices[count] = .{ .ip = ip_val.string, .label = label_val.string };
                count += 1;
            }
            config.device_count = count;
        }
    }

    // Read scalar config values
    if (root.object.get("pollIntervalMs")) |v| {
        if (v == .integer and v.integer > 0) {
            config.poll_interval_ms = std.math.cast(u32, v.integer) orelse 0;
        }
    }
    if (root.object.get("fetchTimeoutMs")) |v| {
        if (v == .integer and v.integer > 0) {
            config.fetch_timeout_ms = std.math.cast(u32, v.integer) orelse 0;
        }
    }
    if (root.object.get("maxApiRows")) |v| {
        if (v == .integer and v.integer > 0) {
            config.max_api_rows = std.math.cast(u32, v.integer) orelse 0;
        }
    }
    if (root.object.get("downsampleBuckets")) |buckets_val| {
        if (buckets_val == .object) {
            var count: usize = 0;
            var it = buckets_val.object.iterator();
            while (it.next()) |entry| {
                if (count >= max_downsample_buckets) break;
                if (entry.value_ptr.* == .integer and entry.value_ptr.integer > 0) {
                    config.downsample_keys[count] = entry.key_ptr.*;
                    config.downsample_values[count] = entry.value_ptr.integer;
                    count += 1;
                }
            }
            config.downsample_bucket_count = count;
        }
    }

    // Read port from ports.zig
    if (root.object.get("ports")) |ports_val| {
        if (ports_val == .object) {
            if (ports_val.object.get("zig")) |v| {
                if (v == .integer and v.integer > 0) {
                    config.port = std.math.cast(u16, v.integer) orelse 0;
                }
            }
        }
    }

    // Validate required fields — collect ALL missing/invalid keys
    var missing_buf: [512]u8 = [_]u8{0} ** 512;
    var missing_stream = std.io.fixedBufferStream(&missing_buf);
    const missing_writer = missing_stream.writer();
    var missing_count: usize = 0;

    if (config.device_count == 0) {
        missing_writer.writeAll("devices") catch {};
        missing_count += 1;
    }
    if (config.poll_interval_ms == 0) {
        if (missing_count > 0) missing_writer.writeAll(", ") catch {};
        missing_writer.writeAll("pollIntervalMs") catch {};
        missing_count += 1;
    }
    if (config.fetch_timeout_ms == 0) {
        if (missing_count > 0) missing_writer.writeAll(", ") catch {};
        missing_writer.writeAll("fetchTimeoutMs") catch {};
        missing_count += 1;
    }
    if (config.max_api_rows == 0) {
        if (missing_count > 0) missing_writer.writeAll(", ") catch {};
        missing_writer.writeAll("maxApiRows") catch {};
        missing_count += 1;
    }
    if (config.downsample_bucket_count == 0) {
        if (missing_count > 0) missing_writer.writeAll(", ") catch {};
        missing_writer.writeAll("downsampleBuckets") catch {};
        missing_count += 1;
    }
    if (config.port == 0) {
        if (missing_count > 0) missing_writer.writeAll(", ") catch {};
        missing_writer.writeAll("ports.zig") catch {};
        missing_count += 1;
    }

    if (missing_count > 0) {
        const missing_str = missing_buf[0..missing_stream.pos];
        std.log.err("[config] FATAL: missing or invalid config keys: {s}", .{missing_str});
        std.process.exit(1);
    }

    // Env var overrides (highest priority)
    if (std.posix.getenv("PORT")) |port_str| {
        config.port = std.fmt.parseInt(u16, port_str, 10) catch {
            std.log.err("[config] FATAL: invalid PORT env var value '{s}'", .{port_str});
            std.process.exit(1);
        };
    }

    if (std.posix.getenv("DB_PATH")) |db_path| {
        config.db_path = db_path;
    }

    return config;
}

fn findAndReadConfigFile(allocator: std.mem.Allocator) ?[]const u8 {
    const paths = [_]struct { path: []const u8, is_env: bool }{
        .{ .path = std.posix.getenv("CONFIG_PATH") orelse "", .is_env = std.posix.getenv("CONFIG_PATH") != null },
        .{ .path = "./airgradientz.json", .is_env = false },
        .{ .path = "../airgradientz.json", .is_env = false },
    };

    for (&paths) |entry| {
        if (entry.path.len == 0) {
            if (entry.is_env) {
                std.log.warn("[config] CONFIG_PATH set but empty", .{});
            }
            continue;
        }

        const file = std.fs.cwd().openFile(entry.path, .{}) catch |e| {
            if (entry.is_env) {
                std.log.warn("[config] CONFIG_PATH set but unreadable: {s}: {}", .{ entry.path, e });
            }
            continue;
        };
        defer file.close();

        const stat = file.stat() catch continue;
        if (stat.size == 0 or stat.size > 1048576) continue;

        const buf = allocator.alloc(u8, stat.size) catch continue;
        const bytes_read = file.readAll(buf) catch continue;
        const content = buf[0..bytes_read];

        if (entry.is_env) {
            std.log.info("[config] Loaded config from CONFIG_PATH: {s}", .{entry.path});
        } else {
            std.log.info("[config] Loaded config from {s}", .{entry.path});
        }
        return content;
    }

    return null;
}
