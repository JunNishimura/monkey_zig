const std = @import("std");
const ast = @import("ast");

pub const Environment = struct {
    store: std.StringHashMap(Object),

    pub fn init(allocator: std.mem.Allocator) !*Environment {
        const env = try allocator.create(Environment);
        env.* = .{
            .store = std.StringHashMap(Object).init(allocator),
        };
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

const ObjectType = enum {
    integer,
    boolean,
    null,
    return_obj,
    error_obj,
    function_obj,
};

pub const Object = union(ObjectType) {
    integer: i64,
    boolean: bool,
    null: void,
    return_obj: *Object,
    error_obj: []const u8,
    function_obj: *Function,

    pub fn getType(self: Object) ObjectType {
        return switch (self) {
            .integer => ObjectType.integer,
            .boolean => ObjectType.boolean,
            .null => ObjectType.null,
            .return_obj => ObjectType.return_obj,
            .error_obj => ObjectType.error_obj,
            .function_obj => ObjectType.function_obj,
        };
    }

    pub fn inspect(self: Object, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            .integer => try std.fmt.allocPrint(allocator, "{d}", .{self.integer}),
            .boolean => try std.fmt.allocPrint(allocator, "{any}", .{self.boolean}),
            .null => "null",
            .return_obj => try self.return_obj.inspect(allocator),
            .error_obj => try std.fmt.allocPrint(allocator, "ERROR: {s}", .{self.error_obj}),
            .function_obj => try self.function_obj.inspect(allocator),
        };
    }

    pub fn deinit(self: Object, allocator: std.mem.Allocator) void {
        switch (self) {
            .error_obj => allocator.free(self.error_obj),
            .function_obj => |func| {
                func.deinit(allocator);
            },
            else => {},
        }
    }
};

pub const Function = struct {
    parameters: []*ast.Identifier,
    body: *ast.BlockStatement,
    env: *Environment,
    str_list: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator, parameters: []*ast.Identifier, body: *ast.BlockStatement, env: *Environment) !*Function {
        const func = try allocator.create(Function);
        func.* = .{
            .parameters = parameters,
            .body = body,
            .env = env,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return func;
    }

    pub fn deinit(self: *Function, allocator: std.mem.Allocator) void {
        self.str_list.deinit();
        allocator.destroy(self);
    }

    pub fn inspect(self: *Function) ![]const u8 {
        try self.str_list.appendSlice("fn(");
        for (self.parameters, 0..) |param, i| {
            if (i > 0) {
                try self.str_list.appendSlice(", ");
            }
            try self.str_list.appendSlice(param.value);
        }
        try self.str_list.appendSlice(") {\n");
        try self.str_list.appendSlice(self.body.string());
        try self.str_list.appendSlice("\n}");
        return self.str_list.items;
    }
};
