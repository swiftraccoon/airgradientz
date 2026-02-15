// Test root: imports modules that contain inline test blocks.
// Run with: zig build test

test {
    _ = @import("db.zig");
    _ = @import("http_server.zig");
}
