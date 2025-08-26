const std = @import("std");
const testing = std.testing;
const Token = @import("token").Token;

const Node = struct {
    ptr: *anyopaque,
    token_literal_fn: *const fn (ptr: *anyopaque) []const u8,
    string_fn: *const fn (ptr: *anyopaque) anyerror![]const u8,
    deinit_fn: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator) void,

    pub fn init(ptr: anytype) Node {
        const T = @TypeOf(ptr);
        const ptr_info = @typeInfo(T);

        const gen = struct {
            pub fn token_literal(pointer: *anyopaque) []const u8 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.token_literal(self);
            }

            pub fn string(pointer: *anyopaque) ![]const u8 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.string(self);
            }

            pub fn deinit(pointer: *anyopaque, allocator: std.mem.Allocator) void {
                const self: T = @ptrCast(@alignCast(pointer));
                ptr_info.pointer.child.deinit(self, allocator);
            }
        };

        return .{
            .ptr = ptr,
            .token_literal_fn = gen.token_literal,
            .string_fn = gen.string,
            .deinit_fn = gen.deinit,
        };
    }

    fn token_literal(self: Node) []const u8 {
        return self.token_literal_fn(self.ptr);
    }

    fn string(self: Node) ![]const u8 {
        return self.string_fn(self.ptr);
    }

    fn deinit(self: Node, allocator: std.mem.Allocator) void {
        self.deinit_fn(self.ptr, allocator);
    }
};

pub const Statement = struct {
    ptr: *anyopaque,
    statement_node_fn: *const fn (ptr: *anyopaque) void,
    node: Node,

    pub fn init(ptr: anytype) Statement {
        const T = @TypeOf(ptr);
        const ptr_info = @typeInfo(T);

        const gen = struct {
            pub fn statement_node(pointer: *anyopaque) void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.statement_node(self);
            }
        };

        return .{
            .ptr = ptr,
            .statement_node_fn = gen.statement_node,
            .node = Node.init(ptr),
        };
    }

    pub fn statement_node(self: Statement) void {
        self.statement_node_fn(self.ptr);
    }

    pub fn deinit(self: Statement, allocator: std.mem.Allocator) void {
        self.node.deinit(allocator);
    }

    pub fn token_literal(self: Statement) []const u8 {
        return self.node.token_literal();
    }

    pub fn string(self: Statement) ![]const u8 {
        return self.node.string();
    }
};

pub const Expression = struct {
    ptr: *anyopaque,
    expression_node_fn: *const fn (ptr: *anyopaque) void,
    node: Node,

    pub fn init(ptr: anytype) Expression {
        const T = @TypeOf(ptr);
        const ptr_info = @typeInfo(T);

        const gen = struct {
            pub fn expression_node(pointer: *anyopaque) void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.expression_node(self);
            }
        };

        return .{
            .ptr = ptr,
            .expression_node_fn = gen.expression_node,
            .node = Node.init(ptr),
        };
    }

    fn expression_node(self: Expression) void {
        self.expression_node_fn(self.ptr);
    }

    fn token_literal(self: Expression) []const u8 {
        return self.node.token_literal();
    }

    fn deinit(self: Expression, allocator: std.mem.Allocator) void {
        self.node.deinit(allocator);
    }

    fn string(self: Expression) ![]const u8 {
        return self.node.string();
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),
    str_list: std.ArrayList(u8),

    fn token_literal(self: *Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].token_literal();
        }
        return "";
    }

    pub fn string(self: *Program) ![]const u8 {
        for (self.statements.items) |stmt| {
            const stmt_str = try stmt.string();
            try self.str_list.appendSlice(stmt_str);
        }

        const out: []const u8 = self.str_list.items;
        return out;
    }

    pub fn init(allocator: std.mem.Allocator) !*Program {
        const p = try allocator.create(Program);
        const stmts = std.ArrayList(Statement).init(allocator);
        const str_list = std.ArrayList(u8).init(allocator);
        p.* = .{
            .statements = stmts,
            .str_list = str_list,
        };
        return p;
    }

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        for (self.statements.items) |stmt| {
            stmt.deinit(allocator);
        }
        self.statements.deinit();
        self.str_list.deinit();
        allocator.destroy(self);
    }
};

pub const LetStatement = struct {
    token: Token,
    name: ?*Identifier,
    value: ?Expression,
    str_list: std.ArrayList(u8),

    fn token_literal(self: *LetStatement) []const u8 {
        return self.token.literal;
    }

    fn statement_node(_: *LetStatement) void {}

    fn string(self: *LetStatement) ![]const u8 {
        try self.str_list.appendSlice(self.token_literal());
        try self.str_list.appendSlice(" ");
        if (self.name) |n| {
            const name_str = try n.string();
            try self.str_list.appendSlice(name_str);
        } else {
            try self.str_list.appendSlice("<no name>");
        }
        try self.str_list.appendSlice(" = ");
        if (self.value) |v| {
            const value_str = try v.string();
            try self.str_list.appendSlice(value_str);
        } else {
            try self.str_list.appendSlice("<no value>");
        }
        try self.str_list.appendSlice(";");
        return self.str_list.items;
    }

    pub fn statement(self: *LetStatement) Statement {
        return Statement.init(self);
    }

    pub fn init(allocator: std.mem.Allocator, token: Token) !*LetStatement {
        const stmt = try allocator.create(LetStatement);
        stmt.* = .{
            .token = token,
            .name = null,
            .value = null,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return stmt;
    }

    pub fn deinit(self: *LetStatement, allocator: std.mem.Allocator) void {
        if (self.name) |n| {
            allocator.destroy(n);
        }
        self.str_list.deinit();
        allocator.destroy(self);
    }
};

pub const ReturnStatement = struct {
    token: Token,
    return_value: ?Expression,
    str_list: std.ArrayList(u8),

    pub fn token_literal(self: *ReturnStatement) []const u8 {
        return self.token.literal;
    }

    fn statement_node(_: *ReturnStatement) void {}

    fn string(self: *ReturnStatement) ![]const u8 {
        try self.str_list.appendSlice(self.token_literal());
        try self.str_list.appendSlice(" ");
        if (self.return_value) |rv| {
            const return_value_str = try rv.string();
            try self.str_list.appendSlice(return_value_str);
        } else {
            try self.str_list.appendSlice("<no return value>");
        }
        try self.str_list.appendSlice(";");
        return self.str_list.items;
    }

    pub fn statement(self: *ReturnStatement) Statement {
        return Statement.init(self);
    }

    pub fn init(allocator: std.mem.Allocator, token: Token) !*ReturnStatement {
        const stmt = try allocator.create(ReturnStatement);
        stmt.* = .{
            .token = token,
            .return_value = null,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return stmt;
    }

    pub fn deinit(self: *ReturnStatement, allocator: std.mem.Allocator) void {
        self.str_list.deinit();
        allocator.destroy(self);
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: ?Expression,

    fn statement_node(_: *ExpressionStatement) void {}
    fn token_literal(self: *ExpressionStatement) []const u8 {
        return self.token.literal;
    }
    fn deinit(self: *ExpressionStatement, allocator: std.mem.Allocator) void {
        if (self.expression) |e| {
            e.deinit(allocator);
        }

        allocator.destroy(self);
    }
    fn string(self: *ExpressionStatement) ![]const u8 {
        if (self.expression) |e| {
            return e.string();
        }
        return "";
    }

    pub fn statement(self: *ExpressionStatement) Statement {
        return Statement.init(self);
    }

    pub fn init(allocator: std.mem.Allocator, token: Token) !*ExpressionStatement {
        const stmt = try allocator.create(ExpressionStatement);
        stmt.* = .{
            .token = token,
            .expression = null,
        };
        return stmt;
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    right: ?Expression,
    str_list: std.ArrayList(u8),

    fn expression_node(_: *PrefixExpression) void {}

    pub fn token_literal(self: *PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *PrefixExpression) ![]const u8 {
        try self.str_list.appendSlice("(");
        try self.str_list.appendSlice(self.operator);

        if (self.right) |r| {
            const right_str = try r.string();
            try self.str_list.appendSlice(right_str);
        }

        try self.str_list.appendSlice(")");
        return self.str_list.items;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, operator: []const u8) !*PrefixExpression {
        const prefix_exp = try allocator.create(PrefixExpression);
        prefix_exp.* = .{
            .token = token,
            .operator = operator,
            .right = null,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return prefix_exp;
    }

    pub fn deinit(self: *PrefixExpression, allocator: std.mem.Allocator) void {
        self.str_list.deinit();
        if (self.right) |r| {
            r.deinit(allocator);
        }
        allocator.destroy(self);
    }

    pub fn expression(self: *PrefixExpression) Expression {
        return Expression.init(self);
    }
};

pub const InfixExpression = struct {
    token: Token,
    left: Expression,
    operator: []const u8,
    right: ?Expression,
    str_list: std.ArrayList(u8),

    fn expression_node(_: *InfixExpression) void {}

    fn token_literal(self: *InfixExpression) []const u8 {
        return self.token.literal;
    }

    fn string(self: *InfixExpression) ![]const u8 {
        try self.str_list.appendSlice("(");
        try self.str_list.appendSlice(try self.left.string());
        try self.str_list.appendSlice(" ");
        try self.str_list.appendSlice(self.operator);
        try self.str_list.appendSlice(" ");
        if (self.right) |r| {
            const right_str = try r.string();
            try self.str_list.appendSlice(right_str);
        }
        try self.str_list.appendSlice(")");
        return self.str_list.items;
    }

    pub fn expression(self: *InfixExpression) Expression {
        return Expression.init(self);
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, operator: []const u8, left: Expression) !*InfixExpression {
        const infix_exp = try allocator.create(InfixExpression);
        infix_exp.* = .{
            .token = token,
            .left = left,
            .operator = operator,
            .right = null,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return infix_exp;
    }

    pub fn deinit(self: *InfixExpression, allocator: std.mem.Allocator) void {
        self.str_list.deinit();
        self.left.deinit(allocator);
        if (self.right) |r| {
            r.deinit(allocator);
        }
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
    pub fn deinit(self: *Identifier, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }
    fn string(self: *Identifier) ![]const u8 {
        return self.value;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, value: []const u8) !*Identifier {
        const ident = try allocator.create(Identifier);
        ident.* = .{
            .token = token,
            .value = value,
        };
        return ident;
    }

    pub fn expression(self: *Identifier) Expression {
        return Expression.init(self);
    }
};

pub const IntegerLiteral = struct {
    token: Token,
    value: ?i64,

    pub fn token_literal(self: *IntegerLiteral) []const u8 {
        return self.token.literal;
    }

    fn expression_node(_: *IntegerLiteral) void {}

    pub fn deinit(self: *IntegerLiteral, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    pub fn string(self: *IntegerLiteral) ![]const u8 {
        return self.token.literal;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token) !*IntegerLiteral {
        const lit = try allocator.create(IntegerLiteral);
        lit.* = .{
            .token = token,
            .value = null,
        };
        return lit;
    }

    pub fn expression(self: *IntegerLiteral) Expression {
        return Expression.init(self);
    }
};

pub const Boolean = struct {
    token: Token,
    value: bool,

    pub fn token_literal(self: *Boolean) []const u8 {
        return self.token.literal;
    }

    fn expression_node(_: *Boolean) void {}

    pub fn string(self: *Boolean) ![]const u8 {
        return self.token.literal;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, value: bool) !*Boolean {
        const b = try allocator.create(Boolean);
        b.* = .{
            .token = token,
            .value = value,
        };
        return b;
    }

    pub fn deinit(self: *Boolean, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    pub fn expression(self: *Boolean) Expression {
        return Expression.init(self);
    }
};

test "test string" {
    const allocator = testing.allocator;

    const program = try Program.init(allocator);
    defer program.deinit(allocator);

    const let_stmt = try LetStatement.init(allocator, Token{ .type = .Let, .literal = "let" });

    let_stmt.name = try Identifier.init(allocator, Token{ .type = .Ident, .literal = "myVar" }, "myVar");
    const value = try Identifier.init(allocator, Token{ .type = .Ident, .literal = "anotherVar" }, "anotherVar");
    defer value.deinit(allocator);
    let_stmt.value = value.expression();
    try program.statements.append(let_stmt.statement());

    const out = try program.string();
    try testing.expect(std.mem.eql(u8, out, "let myVar = anotherVar;"));
}
