const std = @import("std");
const Token = @import("token").Token;

const Node = struct {
    ptr: *anyopaque,
    token_literal_fn: *const fn (ptr: *anyopaque) []const u8,

    fn token_literal(self: Node) []const u8 {
        return self.token_literal_fn(self.ptr);
    }
};

pub const Statement = struct {
    ptr: *anyopaque,
    statement_node_fn: *const fn (ptr: *anyopaque) void,
    deinit_fn: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator) void,
    node: Node,

    pub fn statement_node(self: Statement) void {
        self.statement_node_fn(self.ptr);
    }

    pub fn deinit(self: Statement, allocator: std.mem.Allocator) void {
        self.deinit_fn(self.ptr, allocator);
    }

    pub fn token_literal(self: Statement) []const u8 {
        return self.node.token_literal();
    }
};

const Expression = struct {
    ptr: *anyopaque,
    expression_node_fn: *const fn (ptr: *anyopaque) void,

    fn expression_node(self: Expression) void {
        self.expression_node_fn(self.ptr);
    }

    fn token_literal(self: Expression) []const u8 {
        return self.node.token_literal();
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    fn token_literal(self: *Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].token_literal();
        }
        return "";
    }

    pub fn init(allocator: std.mem.Allocator) !*Program {
        const p = try allocator.create(Program);
        const stmts = std.ArrayList(Statement).init(allocator);
        p.* = .{
            .statements = stmts,
        };
        return p;
    }

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        for (self.statements.items) |stmt| {
            stmt.deinit(allocator);
        }
        self.statements.deinit();
        allocator.destroy(self);
    }
};

pub const LetStatement = struct {
    token: Token,
    name: ?*Identifier,
    value: ?Expression,

    fn token_literal(ptr: *anyopaque) []const u8 {
        const self: *LetStatement = @ptrCast(@alignCast(ptr));
        return self.token.literal;
    }
    fn statement_node(_: *anyopaque) void {}

    pub fn statement(self: *LetStatement) Statement {
        return .{
            .ptr = self,
            .statement_node_fn = statement_node,
            .deinit_fn = deinit,
            .node = .{ .ptr = self, .token_literal_fn = token_literal },
        };
    }

    pub fn init(allocator: std.mem.Allocator, token: Token) !*LetStatement {
        const stmt = try allocator.create(LetStatement);
        stmt.* = .{
            .token = token,
            .name = null,
            .value = null,
        };
        return stmt;
    }

    pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *LetStatement = @ptrCast(@alignCast(ptr));

        if (self.name) |n| {
            allocator.destroy(n);
        }
        allocator.destroy(self);
    }
};

pub const ReturnStatement = struct {
    token: Token,
    return_value: Expression,

    pub fn token_literal(ptr: *anyopaque) []const u8 {
        const self: *ReturnStatement = @ptrCast(@alignCast(ptr));
        return self.token.literal;
    }
    fn statement_node(_: *anyopaque) void {}

    pub fn statement(self: *ReturnStatement) Statement {
        return .{
            .ptr = self,
            .statement_node_fn = statement_node,
            .deinit_fn = deinit,
            .node = .{ .ptr = self, .token_literal_fn = token_literal },
        };
    }

    pub fn init(allocator: std.mem.Allocator, token: Token) !*ReturnStatement {
        const stmt = try allocator.create(ReturnStatement);
        stmt.* = .{
            .token = token,
            .return_value = null,
        };
        return stmt;
    }

    pub fn deinit(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *ReturnStatement = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn token_literal(self: *Identifier) []const u8 {
        return self.token.literal;
    }
    fn expression_node(_: *Identifier) void {}

    pub fn init(allocator: std.mem.Allocator, token: Token, value: []const u8) !*Identifier {
        const ident = try allocator.create(Identifier);
        ident.* = .{
            .token = token,
            .value = value,
        };
        return ident;
    }
};
