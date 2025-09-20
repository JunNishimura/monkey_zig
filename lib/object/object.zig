const std = @import("std");
const ast = @import("ast");

pub const HashKey = struct {
    value: u64,
    type: ObjectType,

    pub fn init(value: u64, obj_type: ObjectType) HashKey {
        return .{
            .value = value,
            .type = obj_type,
        };
    }

    pub fn equals(self: HashKey, other: HashKey) bool {
        return self.value == other.value and self.type == other.type;
    }
};

pub const Environment = struct {
    allocator: std.mem.Allocator,
    store: std.StringHashMap(Object),
    outer: ?*Environment,

    pub fn init(allocator: std.mem.Allocator) !*Environment {
        const env = try allocator.create(Environment);
        env.* = .{
            .allocator = allocator,
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

    pub fn deinit(self: *Environment) void {
        self.store.deinit();
        self.allocator.destroy(self);
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
    array_obj,
    hash_obj,
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

pub const Hashable = union(enum) {
    integer: *Integer,
    string: *String,
    boolean: *Boolean,

    pub fn hashKey(self: Hashable) HashKey {
        return switch (self) {
            .integer => self.integer.hashKey(),
            .string => self.string.hashKey(),
            .boolean => self.boolean.hashKey(),
        };
    }

    pub fn init(obj: Object) ?Hashable {
        return switch (obj.getType()) {
            .integer => {
                const int_ptr: *Integer = @ptrCast(@alignCast(obj.ptr));
                return Hashable{ .integer = int_ptr };
            },
            .string => {
                const str_ptr: *String = @ptrCast(@alignCast(obj.ptr));
                return Hashable{ .string = str_ptr };
            },
            .boolean => {
                const bool_ptr: *Boolean = @ptrCast(@alignCast(obj.ptr));
                return Hashable{ .boolean = bool_ptr };
            },
            else => null,
        };
    }
};

pub const Integer = struct {
    allocator: std.mem.Allocator,
    value: i64,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, value: i64) !*Integer {
        const int_obj = try allocator.create(Integer);
        int_obj.* = .{
            .allocator = allocator,
            .value = value,
            .is_ident = false,
        };
        return int_obj;
    }

    pub fn inspect(self: *Integer, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{d}", .{self.value});
    }

    pub fn getType(_: *Integer) ObjectType {
        return ObjectType.integer;
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

    pub fn hashKey(self: *Integer) HashKey {
        const value: u64 = @intCast(self.value);
        return HashKey.init(value, ObjectType.integer);
    }
};

pub const String = struct {
    allocator: std.mem.Allocator,
    value: []const u8,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, value: []const u8) !*String {
        const str_value = try allocator.dupe(u8, value);
        const str_obj = try allocator.create(String);
        str_obj.* = .{
            .allocator = allocator,
            .value = str_value,
            .is_ident = false,
        };
        return str_obj;
    }

    pub fn inspect(self: *String, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{s}", .{self.value});
    }

    pub fn getType(_: *String) ObjectType {
        return ObjectType.string;
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

    pub fn hashKey(self: *String) HashKey {
        var hasher = std.hash.Fnv1a_64.init();
        hasher.update(self.value);
        return HashKey.init(hasher.final(), ObjectType.string);
    }
};

pub const Boolean = struct {
    allocator: std.mem.Allocator,
    value: bool,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, value: bool) !*Boolean {
        const bool_obj = try allocator.create(Boolean);
        bool_obj.* = .{
            .allocator = allocator,
            .value = value,
            .is_ident = false,
        };
        return bool_obj;
    }

    pub fn inspect(self: *Boolean, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "{any}", .{self.value});
    }

    pub fn getType(_: *Boolean) ObjectType {
        return ObjectType.boolean;
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

    pub fn hashKey(self: *Boolean) HashKey {
        var value: u8 = 0;
        if (self.value) {
            value = 1;
        }
        return HashKey.init(@as(u64, value), ObjectType.boolean);
    }
};

pub const Null = struct {
    allocator: std.mem.Allocator,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator) !*Null {
        const null_obj = try allocator.create(Null);
        null_obj.* = .{
            .allocator = allocator,
            .is_ident = false,
        };
        return null_obj;
    }

    pub fn inspect(_: *Null, _: std.mem.Allocator) ![]const u8 {
        return "null";
    }

    pub fn getType(_: *Null) ObjectType {
        return ObjectType.null;
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
    message: []const u8,

    pub fn init(allocator: std.mem.Allocator, comptime format: []const u8, args: anytype) !*Error {
        const error_message = try std.fmt.allocPrint(allocator, format, args);
        const error_obj = try allocator.create(Error);
        error_obj.* = .{
            .allocator = allocator,
            .message = error_message,
        };
        return error_obj;
    }

    pub fn inspect(self: *Error, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "ERROR: {s}", .{self.message});
    }

    pub fn getType(_: *Error) ObjectType {
        return ObjectType.error_obj;
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
    value: ?Object,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, value: Object) !*Return {
        const return_obj = try allocator.create(Return);
        return_obj.* = .{
            .allocator = allocator,
            .value = value,
            .is_ident = false,
        };
        return return_obj;
    }

    pub fn inspect(self: *Return, allocator: std.mem.Allocator) ![]const u8 {
        return self.value.?.inspect(allocator);
    }

    pub fn getType(_: *Return) ObjectType {
        return ObjectType.return_obj;
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
    parameters: []*ast.Identifier,
    body: *ast.BlockStatement,
    env: *Environment,
    extended_env: ?*Environment,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, parameters: []*ast.Identifier, body: *ast.BlockStatement, env: *Environment) !*Function {
        const func = try allocator.create(Function);
        func.* = .{
            .allocator = allocator,
            .parameters = parameters,
            .body = body,
            .env = env,
            .extended_env = null,
            .is_ident = false,
        };
        return func;
    }

    pub fn getType(_: *Function) ObjectType {
        return ObjectType.function_obj;
    }

    pub fn deinit(self: *Function) void {
        if (self.extended_env) |ext_env| {
            ext_env.deinit();
        }
        self.allocator.destroy(self);
    }

    pub fn inspect(self: *Function, allocator: std.mem.Allocator) ![]const u8 {
        var result = try std.fmt.allocPrint(allocator, "fn(", .{});
        for (self.parameters, 0..) |param, i| {
            if (i > 0) {
                result = try std.fmt.allocPrint(allocator, "{s}, ", .{result});
            }
            result = try std.fmt.allocPrint(allocator, "{s}{s}", .{ result, param.value });
        }
        result = try std.fmt.allocPrint(allocator, "{s}) {{\n{s}\n}}", .{ result, try self.body.string() });
        return result;
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
    func: BuiltinFn,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, func: BuiltinFn) !*Builtin {
        const builtin = try allocator.create(Builtin);
        builtin.* = .{
            .allocator = allocator,
            .func = func,
            .is_ident = false,
        };
        return builtin;
    }

    pub fn inspect(_: *Builtin, _: std.mem.Allocator) ![]const u8 {
        return "builtin";
    }

    pub fn getType(_: *Builtin) ObjectType {
        return ObjectType.builtin_obj;
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

pub const Array = struct {
    allocator: std.mem.Allocator,
    elements: []Object,
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator, elements: []Object) !*Array {
        const arr_elements = try allocator.alloc(Object, elements.len);
        for (elements, 0..) |elem, i| {
            arr_elements[i] = elem;
        }
        const array_obj = try allocator.create(Array);
        array_obj.* = .{
            .allocator = allocator,
            .elements = arr_elements,
            .is_ident = false,
        };
        return array_obj;
    }

    pub fn inspect(self: *Array, allocator: std.mem.Allocator) ![]const u8 {
        var result = try std.fmt.allocPrint(allocator, "[", .{});
        for (self.elements, 0..) |elem, i| {
            if (i > 0) {
                result = try std.fmt.allocPrint(allocator, "{s}, ", .{result});
            }
            result = try std.fmt.allocPrint(allocator, "{s}{s}", .{ result, try elem.inspect(allocator) });
        }
        result = try std.fmt.allocPrint(allocator, "{s}]", .{result});
        return result;
    }

    pub fn getType(_: *Array) ObjectType {
        return ObjectType.array_obj;
    }

    pub fn deinit(self: *Array) void {
        self.allocator.free(self.elements);
        self.allocator.destroy(self);
    }

    pub fn object(self: *Array) Object {
        return Object.init(self);
    }

    pub fn setIsIdent(self: *Array, is_ident: bool) void {
        self.is_ident = is_ident;
    }

    pub fn isIdent(self: *Array) bool {
        return self.is_ident;
    }

    pub fn setEnv(_: *Array, _: *Environment) void {}
};

pub const HashPair = struct {
    key: Object,
    value: Object,
};

pub const Hash = struct {
    allocator: std.mem.Allocator,
    pairs: std.AutoHashMap(HashKey, HashPair),
    is_ident: bool,

    pub fn init(allocator: std.mem.Allocator) !*Hash {
        const hash = try allocator.create(Hash);
        hash.* = .{
            .allocator = allocator,
            .pairs = std.AutoHashMap(HashKey, HashPair).init(allocator),
            .is_ident = false,
        };
        return hash;
    }

    pub fn inspect(self: *Hash, allocator: std.mem.Allocator) ![]const u8 {
        var result = std.ArrayList(u8).init(allocator);
        defer result.deinit();

        try result.append('{');
        var first = true;
        var it = self.pairs.iterator();
        while (it.next()) |pair| {
            if (!first) {
                try result.appendSlice(", ");
            } else {
                first = false;
            }
            const key_str = try pair.value_ptr.*.key.inspect(allocator);
            const value_str = try pair.value_ptr.*.value.inspect(allocator);
            try result.appendSlice(key_str);
            try result.appendSlice(" : ");
            try result.appendSlice(value_str);
            allocator.free(key_str);
            allocator.free(value_str);
        }
        try result.append('}');

        return result.toOwnedSlice();
    }

    pub fn getType(_: *Hash) ObjectType {
        return ObjectType.hash_obj;
    }

    pub fn deinit(self: *Hash) void {
        self.pairs.deinit();
        self.allocator.destroy(self);
    }

    pub fn object(self: *Hash) Object {
        return Object.init(self);
    }

    pub fn setIsIdent(self: *Hash, is_ident: bool) void {
        self.is_ident = is_ident;
    }

    pub fn isIdent(self: *Hash) bool {
        return self.is_ident;
    }

    pub fn setEnv(_: *Hash, _: *Environment) void {}
};

test "string hash key" {
    const allocator = std.testing.allocator;

    const str1 = try String.init(allocator, "Hello World");
    defer str1.deinit();
    const str2 = try String.init(allocator, "Hello World");
    defer str2.deinit();
    const str3 = try String.init(allocator, "My name is John");
    defer str3.deinit();
    const str4 = try String.init(allocator, "My name is John");
    defer str4.deinit();

    try std.testing.expect(str1.hashKey().equals(str2.hashKey()));
    try std.testing.expect(str3.hashKey().equals(str4.hashKey()));
    try std.testing.expect(!str1.hashKey().equals(str3.hashKey()));
}
