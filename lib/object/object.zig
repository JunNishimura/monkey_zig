const std = @import("std");

const ObjectType = enum {
    integer,
    boolean,
    null,
};

pub const Object = union(ObjectType) {
    integer: i64,
    boolean: bool,
    null: void,

    pub fn getType(self: Object) ObjectType {
        return switch (self) {
            .integer => ObjectType.integer,
            .boolean => ObjectType.boolean,
            .null => ObjectType.null,
        };
    }

    pub fn inspect(self: Object, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            .integer => try std.fmt.allocPrint(allocator, "{d}", .{self.integer}),
            .boolean => try std.fmt.allocPrint(allocator, "{any}", .{self.boolean}),
            .null => "null",
        };
    }
};
