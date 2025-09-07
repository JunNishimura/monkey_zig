const std = @import("std");

const ObjectType = enum {
    integer,
    boolean,
    null,
    return_obj,
    error_obj,
};

pub const Object = union(ObjectType) {
    integer: i64,
    boolean: bool,
    null: void,
    return_obj: *Object,
    error_obj: []const u8,

    pub fn getType(self: Object) ObjectType {
        return switch (self) {
            .integer => ObjectType.integer,
            .boolean => ObjectType.boolean,
            .null => ObjectType.null,
            .return_obj => ObjectType.return_obj,
            .error_obj => ObjectType.error_obj,
        };
    }

    pub fn inspect(self: Object, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            .integer => try std.fmt.allocPrint(allocator, "{d}", .{self.integer}),
            .boolean => try std.fmt.allocPrint(allocator, "{any}", .{self.boolean}),
            .null => "null",
            .return_obj => try self.return_obj.inspect(allocator),
            .error_obj => try std.fmt.allocPrint(allocator, "ERROR: {s}", .{self.error_obj}),
        };
    }

    pub fn deinit(self: Object, allocator: std.mem.Allocator) void {
        switch (self) {
            .error_obj => allocator.free(self.error_obj),
            else => {},
        }
    }
};
