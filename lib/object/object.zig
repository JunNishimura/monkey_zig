const std = @import("std");
const ast = @import("ast");

pub const Environment = struct {
    store: std.StringHashMap(Object),
    outer: ?*Environment,

    pub fn init(allocator: std.mem.Allocator) !*Environment {
        const env = try allocator.create(Environment);
        env.* = .{
            .store = std.StringHashMap(Object).init(allocator),
            .outer = null,
        };
        return env;
    }

    pub fn initEnclosed(allocator: std.mem.Allocator, outer: *Environment) !*Environment {
        const env = try Environment.init(allocator);
        env.outer = outer;
        return env;
    }

    pub fn deinit(self: *Environment, allocator: std.mem.Allocator) void {
        self.store.deinit();
        allocator.destroy(self);
    }

    pub fn get(self: *Environment, key: []const u8) ?Object {
        const value = self.store.get(key);
        if (value) |v| {
            return v;
        }
        if (self.outer) |outer| {
            return outer.get(key);
        }
        return null;
    }

    pub fn set(self: *Environment, key: []const u8, value: Object) !void {
        try self.store.put(key, value);
    }
};

const ObjectType = enum {
    integer,
    string,
    boolean,
    null,
    return_obj,
    error_obj,
    function_obj,
    builtin_obj,
};

pub const Object = struct {
    ptr: *anyopaque,
    inspect_fn: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator) anyerror![]const u8,
    get_type_fn: *const fn (ptr: *anyopaque) ObjectType,
    deinit_fn: *const fn (ptr: *anyopaque) void,
    set_is_ident_fn: *const fn (ptr: *anyopaque, is_ident: bool) void,
    is_ident_fn: *const fn (ptr: *anyopaque) bool,
    set_env_fn: *const fn (ptr: *anyopaque, env: *Environment) void,

    pub fn init(ptr: anytype) Object {
        const T = @TypeOf(ptr);
        const ptr_info = @typeInfo(T);

        const gen = struct {
            pub fn inspect(pointer: *anyopaque, allocator: std.mem.Allocator) ![]const u8 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.inspect(self, allocator);
            }
            pub fn getType(pointer: *anyopaque) ObjectType {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.getType(self);
            }
            pub fn deinit(pointer: *anyopaque) void {
                const self: T = @ptrCast(@alignCast(pointer));
                ptr_info.pointer.child.deinit(self);
            }
            pub fn setIsIdent(pointer: *anyopaque, is_ident: bool) void {
                const self: T = @ptrCast(@alignCast(pointer));
                ptr_info.pointer.child.setIsIdent(self, is_ident);
            }
            pub fn isIdent(pointer: *anyopaque) bool {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.isIdent(self);
            }
            pub fn setEnv(pointer: *anyopaque, env: *Environment) void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.setEnv(self, env);
            }
        };

        return .{
            .ptr = ptr,
            .inspect_fn = gen.inspect,
            .get_type_fn = gen.getType,
            .deinit_fn = gen.deinit,
            .set_is_ident_fn = gen.setIsIdent,
            .is_ident_fn = gen.isIdent,
            .set_env_fn = gen.setEnv,
        };
    }

    pub fn inspect(self: Object, allocator: std.mem.Allocator) ![]const u8 {
        return self.inspect_fn(self.ptr, allocator);
    }

    pub fn getType(self: Object) ObjectType {
        return self.get_type_fn(self.ptr);
    }

    pub fn deinit(self: Object) void {
        self.deinit_fn(self.ptr);
    }

    pub fn isError(self: Object) bool {
        return self.getType() == ObjectType.error_obj;
    }

    pub fn setIsIdent(self: Object, is_ident: bool) void {
        self.set_is_ident_fn(self.ptr, is_ident);
    }

    pub fn isIdent(self: Object) bool {
        return self.is_ident_fn(self.ptr);
    }

    pub fn setEnv(self: Object, env: *Environment) void {
        self.set_env_fn(self.ptr, env);
    }
};

pub const Integer = struct {
    allocator: std.mem.Allocator,
    type: ObjectType,
    value: i64,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, value: i64) !*Integer {
        const int_obj = try allocator.create(Integer);
        int_obj.* = .{
            .allocator = allocator,
            .type = ObjectType.integer,
            .value = value,
            .is_ident = false,
        };
        return int_obj;
    }

    pub fn inspect(self: *Integer, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{d}", .{self.value});
    }

    pub fn getType(self: *Integer) ObjectType {
        return self.type;
    }

    pub fn deinit(self: *Integer) void {
        self.allocator.destroy(self);
    }

    pub fn object(self: *Integer) Object {
        return Object.init(self);
    }

    pub fn setIsIdent(self: *Integer, is_ident: bool) void {
        self.is_ident = is_ident;
    }

    pub fn isIdent(self: *Integer) bool {
        return self.is_ident;
    }

    pub fn setEnv(_: *Integer, _: *Environment) void {}
};

pub const String = struct {
    allocator: std.mem.Allocator,
    type: ObjectType,
    value: []const u8,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, value: []const u8) !*String {
        const str_value = try allocator.dupe(u8, value);
        const str_obj = try allocator.create(String);
        str_obj.* = .{
            .allocator = allocator,
            .type = ObjectType.string,
            .value = str_value,
            .is_ident = false,
        };
        return str_obj;
    }

    pub fn inspect(self: *String, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{s}", .{self.value});
    }

    pub fn getType(self: *String) ObjectType {
        return self.type;
    }

    pub fn deinit(self: *String) void {
        self.allocator.free(self.value);
        self.allocator.destroy(self);
    }

    pub fn object(self: *String) Object {
        return Object.init(self);
    }

    pub fn setIsIdent(self: *String, is_ident: bool) void {
        self.is_ident = is_ident;
    }

    pub fn isIdent(self: *String) bool {
        return self.is_ident;
    }

    pub fn setEnv(_: *String, _: *Environment) void {}
};

pub const Boolean = struct {
    allocator: std.mem.Allocator,
    type: ObjectType,
    value: bool,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, value: bool) !*Boolean {
        const bool_obj = try allocator.create(Boolean);
        bool_obj.* = .{
            .allocator = allocator,
            .type = ObjectType.boolean,
            .value = value,
            .is_ident = false,
        };
        return bool_obj;
    }

    pub fn inspect(self: *Boolean, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{any}", .{self.value});
    }

    pub fn getType(self: *Boolean) ObjectType {
        return self.type;
    }

    pub fn deinit(self: *Boolean) void {
        self.allocator.destroy(self);
    }

    pub fn object(self: *Boolean) Object {
        return Object.init(self);
    }

    pub fn setIsIdent(self: *Boolean, is_ident: bool) void {
        self.is_ident = is_ident;
    }

    pub fn isIdent(self: *Boolean) bool {
        return self.is_ident;
    }

    pub fn setEnv(_: *Boolean, _: *Environment) void {}
};

pub const Null = struct {
    allocator: std.mem.Allocator,
    type: ObjectType,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator) !*Null {
        const null_obj = try allocator.create(Null);
        null_obj.* = .{
            .allocator = allocator,
            .type = ObjectType.null,
            .is_ident = false,
        };
        return null_obj;
    }

    pub fn inspect(_: *Null, _: std.mem.Allocator) ![]const u8 {
        return "null";
    }

    pub fn getType(self: *Null) ObjectType {
        return self.type;
    }

    pub fn deinit(self: *Null) void {
        self.allocator.destroy(self);
    }

    pub fn object(self: *Null) Object {
        return Object.init(self);
    }

    pub fn setIsIdent(self: *Null, is_ident: bool) void {
        self.is_ident = is_ident;
    }

    pub fn isIdent(self: *Null) bool {
        return self.is_ident;
    }

    pub fn setEnv(_: *Null, _: *Environment) void {}
};

pub const Error = struct {
    allocator: std.mem.Allocator,
    type: ObjectType,
    message: []const u8,

    pub fn init(allocator: std.mem.Allocator, comptime format: []const u8, args: anytype) !*Error {
        const error_message = try std.fmt.allocPrint(allocator, format, args);
        const error_obj = try allocator.create(Error);
        error_obj.* = .{
            .allocator = allocator,
            .type = ObjectType.error_obj,
            .message = error_message,
        };
        return error_obj;
    }

    pub fn inspect(self: *Error, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "ERROR: {s}", .{self.message});
    }

    pub fn getType(self: *Error) ObjectType {
        return self.type;
    }

    pub fn deinit(self: *Error) void {
        self.allocator.free(self.message);
        self.allocator.destroy(self);
    }

    pub fn object(self: *Error) Object {
        return Object.init(self);
    }

    pub fn setIsIdent(_: *Error, _: bool) void {
        // do nothing
    }

    pub fn isIdent(_: *Error) bool {
        return false;
    }

    pub fn setEnv(_: *Error, _: *Environment) void {}
};

pub const Return = struct {
    allocator: std.mem.Allocator,
    type: ObjectType,
    value: ?Object,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, value: Object) !*Return {
        const return_obj = try allocator.create(Return);
        return_obj.* = .{
            .allocator = allocator,
            .type = ObjectType.return_obj,
            .value = value,
            .is_ident = false,
        };
        return return_obj;
    }

    pub fn inspect(self: *Return, allocator: std.mem.Allocator) ![]const u8 {
        return self.value.?.inspect(allocator);
    }

    pub fn getType(self: *Return) ObjectType {
        return self.type;
    }

    pub fn deinit(self: *Return) void {
        if (self.value) |v| {
            v.deinit();
        }
        self.allocator.destroy(self);
    }

    pub fn object(self: *Return) Object {
        return Object.init(self);
    }

    pub fn setIsIdent(self: *Return, is_ident: bool) void {
        self.is_ident = is_ident;
    }

    pub fn isIdent(self: *Return) bool {
        return self.is_ident;
    }

    pub fn setEnv(_: *Return, _: *Environment) void {}
};

pub const Function = struct {
    allocator: std.mem.Allocator,
    type: ObjectType,
    parameters: []*ast.Identifier,
    body: *ast.BlockStatement,
    env: *Environment,
    extended_env: ?*Environment,
    str_list: std.ArrayList(u8),
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, parameters: []*ast.Identifier, body: *ast.BlockStatement, env: *Environment) !*Function {
        const func = try allocator.create(Function);
        func.* = .{
            .allocator = allocator,
            .type = ObjectType.function_obj,
            .parameters = parameters,
            .body = body,
            .env = env,
            .extended_env = null,
            .str_list = std.ArrayList(u8).init(allocator),
            .is_ident = false,
        };
        return func;
    }

    pub fn getType(self: *Function) ObjectType {
        return self.type;
    }

    pub fn deinit(self: *Function) void {
        if (self.extended_env) |ext_env| {
            ext_env.deinit(self.allocator);
        }
        self.str_list.deinit();
        self.allocator.destroy(self);
    }

    pub fn inspect(self: *Function, _: std.mem.Allocator) ![]const u8 {
        try self.str_list.appendSlice("fn(");
        for (self.parameters, 0..) |param, i| {
            if (i > 0) {
                try self.str_list.appendSlice(", ");
            }
            try self.str_list.appendSlice(param.value);
        }
        try self.str_list.appendSlice(") {\n");
        try self.str_list.appendSlice(try self.body.string());
        try self.str_list.appendSlice("\n}");
        return self.str_list.items;
    }

    pub fn object(self: *Function) Object {
        return Object.init(self);
    }

    pub fn setIsIdent(self: *Function, is_ident: bool) void {
        self.is_ident = is_ident;
    }

    pub fn isIdent(self: *Function) bool {
        return self.is_ident;
    }

    pub fn setEnv(self: *Function, env: *Environment) void {
        self.extended_env = env;
    }
};

const ObjectError = error{OutOfMemory};
pub const BuiltinFn = *const fn (allocator: std.mem.Allocator, args: []Object) ObjectError!?Object;

pub const Builtin = struct {
    allocator: std.mem.Allocator,
    type: ObjectType,
    func: BuiltinFn,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, func: BuiltinFn) !*Builtin {
        const builtin = try allocator.create(Builtin);
        builtin.* = .{
            .allocator = allocator,
            .type = ObjectType.builtin_obj,
            .func = func,
            .is_ident = false,
        };
        return builtin;
    }

    pub fn inspect(_: *Builtin, _: std.mem.Allocator) ![]const u8 {
        return "builtin";
    }

    pub fn getType(self: *Builtin) ObjectType {
        return self.type;
    }

    pub fn deinit(self: *Builtin) void {
        self.allocator.destroy(self);
    }

    pub fn object(self: *Builtin) Object {
        return Object.init(self);
    }

    pub fn setIsIdent(self: *Builtin, is_ident: bool) void {
        self.is_ident = is_ident;
    }

    pub fn isIdent(self: *Builtin) bool {
        return self.is_ident;
    }

    pub fn setEnv(_: *Builtin, _: *Environment) void {}
};
