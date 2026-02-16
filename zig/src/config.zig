const std = @import("std");

pub const max_devices = 8;

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
};

const empty_device = DeviceConfig{ .ip = "", .label = "" };

pub fn fromEnv() Config {
    var config = Config{
        .port = 3012,
        .db_path = "./airgradientz.db",
        .devices = [_]DeviceConfig{empty_device} ** max_devices,
        .device_count = 2,
        .poll_interval_ms = 15000,
        .fetch_timeout_ms = 5000,
        .max_api_rows = 10000,
    };

    config.devices[0] = .{ .ip = "192.168.88.6", .label = "outdoor" };
    config.devices[1] = .{ .ip = "192.168.88.159", .label = "indoor" };

    // 2. Config file overrides
    loadConfigFile(&config);

    // 3. Env var overrides (highest priority)
    if (std.posix.getenv("PORT")) |port_str| {
        config.port = std.fmt.parseInt(u16, port_str, 10) catch {
            std.log.warn("[config] invalid PORT value '{s}', using default 3012", .{port_str});
            return config;
        };
    }

    if (std.posix.getenv("DB_PATH")) |db_path| {
        config.db_path = db_path;
    }

    return config;
}

fn loadConfigFile(config: *Config) void {
    const allocator = std.heap.page_allocator;

    const content = findAndReadConfigFile(allocator) orelse return;

    const parsed = std.json.parseFromSlice(std.json.Value, allocator, content, .{}) catch |e| {
        std.log.warn("[config] JSON parse error: {}", .{e});
        return;
    };

    const root = parsed.value;
    if (root != .object) return;

    // Apply defaults first (lower priority)
    if (root.object.get("defaults")) |defaults_val| {
        if (defaults_val == .object) {
            applyConfigValues(config, defaults_val.object);
        }
    }

    // Apply top-level overrides (higher priority)
    applyConfigValues(config, root.object);

    // Port from ports.zig
    if (root.object.get("ports")) |ports_val| {
        if (ports_val == .object) {
            if (ports_val.object.get("zig")) |v| {
                if (v == .integer and v.integer > 0) {
                    config.port = std.math.cast(u16, v.integer) orelse config.port;
                }
            }
        }
    }
}

fn applyConfigValues(config: *Config, obj: std.json.ObjectMap) void {
    if (obj.get("devices")) |devices_val| {
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
            if (count > 0) {
                config.device_count = count;
                var i = count;
                while (i < max_devices) : (i += 1) {
                    config.devices[i] = empty_device;
                }
            }
        }
    }

    if (obj.get("pollIntervalMs")) |v| {
        if (v == .integer and v.integer > 0) {
            config.poll_interval_ms = std.math.cast(u32, v.integer) orelse config.poll_interval_ms;
        }
    }
    if (obj.get("fetchTimeoutMs")) |v| {
        if (v == .integer and v.integer > 0) {
            config.fetch_timeout_ms = std.math.cast(u32, v.integer) orelse config.fetch_timeout_ms;
        }
    }
    if (obj.get("maxApiRows")) |v| {
        if (v == .integer and v.integer > 0) {
            config.max_api_rows = std.math.cast(u32, v.integer) orelse config.max_api_rows;
        }
    }
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
