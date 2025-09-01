const std = @import("std");

const ObjectType = enum {
    Integer,
    Boolean,
    Null,
};

pub const Object = struct {
    ptr: *anyopaque,
    get_type_fn: *const fn (ptr: *anyopaque) ObjectType,
    inspect_fn: *const fn (ptr: *anyopaque) anyerror![]const u8,

    pub fn init(ptr: anytype) Object {
        const T = @TypeOf(ptr);
        const ptr_info = @typeInfo(T);

        const gen = struct {
            pub fn getType(pointer: *anyopaque) ObjectType {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.getType(self);
            }

            pub fn inspect(pointer: *anyopaque) anyerror![]const u8 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.inspect(self);
            }
        };

        return .{
            .ptr = ptr,
            .get_type_fn = gen.getType,
            .inspect_fn = gen.inspect,
        };
    }

    pub fn getType(self: *Object) ObjectType {
        return self.get_type_fn(self.ptr);
    }

    pub fn inspect(self: *Object) anyerror![]const u8 {
        return self.inspect_fn(self.ptr);
    }
};

pub const Integer = struct {
    value: i64,

    pub fn getType(_: *Integer) ObjectType {
        return ObjectType.Integer;
    }

    pub fn inspect(self: *Integer, allocator: std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(allocator, "{d}", self.value);
    }

    pub fn init(allocator: std.mem.Allocator, value: i64) !*Integer {
        const integer_obj = try allocator.create(Integer);
        integer_obj.* = .{
            .value = value,
        };
        return integer_obj;
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn getType(_: *Boolean) ObjectType {
        return ObjectType.Boolean;
    }

    pub fn inspect(self: *Boolean, allocator: std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(allocator, "{b}", self.value);
    }

    pub fn init(allocator: std.mem.Allocator, value: bool) !*Boolean {
        const boolean_obj = try allocator.create(Boolean);
        boolean_obj.* = .{
            .value = value,
        };
        return boolean_obj;
    }
};

pub const Null = struct {
    pub fn getType(_: *Null) ObjectType {
        return ObjectType.Null;
    }

    pub fn inspect(_: *Null, _: std.mem.Allocator) ![]const u8 {
        return "null";
    }

    pub fn init(allocator: std.mem.Allocator) !*Null {
        return try allocator.create(Null);
    }
};
