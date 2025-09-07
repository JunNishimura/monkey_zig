const std = @import("std");
const Object = @import("object").Object;

pub const Environment = struct {
    store: std.StringHashMap(Object),

    pub fn init(allocator: std.mem.Allocator) !*Environment {
        const env = try allocator.create(Environment);
        env.store = std.StringHashMap(Object).init(allocator);
        return env;
    }

    pub fn deinit(self: *Environment, allocator: std.mem.Allocator) void {
        var it = self.store.keyIterator();
        while (it.next()) |key| {
            allocator.free(key.*);
        }
        self.store.deinit();
        allocator.destroy(self);
    }

    pub fn get(self: *Environment, key: []const u8) ?Object {
        return self.store.get(key);
    }

    pub fn set(self: *Environment, key: []const u8, value: Object) !void {
        try self.store.put(key, value);
    }
};
