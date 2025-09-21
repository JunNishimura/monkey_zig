const std = @import("std");
const testing = std.testing;
const tok = @import("token");
const Token = tok.Token;

const NodeType = enum {
    Program,
    LetStatement,
    ReturnStatement,
    ExpressionStatement,
    BlockStatement,
    Identifier,
    IntegerLiteral,
    StringLiteral,
    Boolean,
    PrefixExpression,
    InfixExpression,
    IfExpression,
    FunctionLiteral,
    CallExpression,
    ArrayLiteral,
    IndexExpression,
    HashLiteral,
};

pub const Node = struct {
    ptr: *anyopaque,
    node_type_fn: *const fn (ptr: *anyopaque) NodeType,
    token_literal_fn: *const fn (ptr: *anyopaque) []const u8,
    string_fn: *const fn (ptr: *anyopaque) anyerror![]const u8,
    deinit_fn: *const fn (ptr: *anyopaque) void,

    pub fn init(ptr: anytype) Node {
        const T = @TypeOf(ptr);
        const ptr_info = @typeInfo(T);

        const gen = struct {
            pub fn nodeType(pointer: *anyopaque) NodeType {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.nodeType(self);
            }

            pub fn tokenLiteral(pointer: *anyopaque) []const u8 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.tokenLiteral(self);
            }

            pub fn string(pointer: *anyopaque) ![]const u8 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.string(self);
            }

            pub fn deinit(pointer: *anyopaque) void {
                const self: T = @ptrCast(@alignCast(pointer));
                ptr_info.pointer.child.deinit(self);
            }
        };

        return .{
            .ptr = ptr,
            .node_type_fn = gen.nodeType,
            .token_literal_fn = gen.tokenLiteral,
            .string_fn = gen.string,
            .deinit_fn = gen.deinit,
        };
    }

    pub fn nodeType(self: Node) NodeType {
        return self.node_type_fn(self.ptr);
    }

    pub fn tokenLiteral(self: Node) []const u8 {
        return self.token_literal_fn(self.ptr);
    }

    pub fn string(self: Node) ![]const u8 {
        return self.string_fn(self.ptr);
    }

    pub fn deinit(self: Node) void {
        self.deinit_fn(self.ptr);
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
            pub fn statementNode(pointer: *anyopaque) void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.statementNode(self);
            }
        };

        return .{
            .ptr = ptr,
            .statement_node_fn = gen.statementNode,
            .node = Node.init(ptr),
        };
    }

    pub fn statementNode(self: Statement) void {
        self.statement_node_fn(self.ptr);
    }

    pub fn deinit(self: Statement) void {
        self.node.deinit();
    }

    pub fn nodeType(self: Statement) NodeType {
        return self.node.nodeType();
    }

    pub fn tokenLiteral(self: Statement) []const u8 {
        return self.node.tokenLiteral();
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
            pub fn expressionNode(pointer: *anyopaque) void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.expressionNode(self);
            }
        };

        return .{
            .ptr = ptr,
            .expression_node_fn = gen.expressionNode,
            .node = Node.init(ptr),
        };
    }

    fn expressionNode(self: Expression) void {
        self.expression_node_fn(self.ptr);
    }

    pub fn nodeType(self: Expression) NodeType {
        return self.node.nodeType();
    }

    fn tokenLiteral(self: Expression) []const u8 {
        return self.node.tokenLiteral();
    }

    fn deinit(self: Expression) void {
        self.node.deinit();
    }

    fn string(self: Expression) ![]const u8 {
        return self.node.string();
    }
};

pub const Program = struct {
    allocator: std.mem.Allocator,
    statements: std.ArrayList(Statement),
    str_list: std.ArrayList(u8),

    pub fn nodeType(_: *Program) NodeType {
        return .Program;
    }

    fn tokenLiteral(self: *Program) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].tokenLiteral();
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
            .allocator = allocator,
            .statements = stmts,
            .str_list = str_list,
        };
        return p;
    }

    pub fn deinit(self: *Program) void {
        for (self.statements.items) |stmt| {
            stmt.deinit();
        }
        self.statements.deinit();
        self.str_list.deinit();
        self.allocator.destroy(self);
    }

    pub fn node(self: *Program) Node {
        return Node.init(self);
    }
};

pub const LetStatement = struct {
    allocator: std.mem.Allocator,
    token: Token,
    name: *Identifier,
    value: Expression,
    str_list: std.ArrayList(u8),

    pub fn nodeType(_: *LetStatement) NodeType {
        return .LetStatement;
    }

    fn tokenLiteral(self: *LetStatement) []const u8 {
        return self.token.literal;
    }

    fn statementNode(_: *LetStatement) void {}

    fn string(self: *LetStatement) ![]const u8 {
        try self.str_list.appendSlice(self.tokenLiteral());
        try self.str_list.appendSlice(" ");
        try self.str_list.appendSlice(try self.name.string());
        try self.str_list.appendSlice(" = ");
        try self.str_list.appendSlice(try self.value.string());
        try self.str_list.appendSlice(";");
        return self.str_list.items;
    }

    pub fn statement(self: *LetStatement) Statement {
        return Statement.init(self);
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, name: *Identifier, value: Expression) !*LetStatement {
        const stmt = try allocator.create(LetStatement);
        stmt.* = .{
            .allocator = allocator,
            .token = token,
            .name = name,
            .value = value,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return stmt;
    }

    pub fn deinit(self: *LetStatement) void {
        self.name.deinit();
        self.value.deinit();
        self.str_list.deinit();
        self.allocator.destroy(self);
    }
};

pub const ReturnStatement = struct {
    allocator: std.mem.Allocator,
    token: Token,
    return_value: Expression,
    str_list: std.ArrayList(u8),

    pub fn nodeType(_: *ReturnStatement) NodeType {
        return .ReturnStatement;
    }

    pub fn tokenLiteral(self: *ReturnStatement) []const u8 {
        return self.token.literal;
    }

    fn statementNode(_: *ReturnStatement) void {}

    fn string(self: *ReturnStatement) ![]const u8 {
        try self.str_list.appendSlice(self.tokenLiteral());
        try self.str_list.appendSlice(" ");
        try self.str_list.appendSlice(try self.return_value.string());
        try self.str_list.appendSlice(";");
        return self.str_list.items;
    }

    pub fn statement(self: *ReturnStatement) Statement {
        return Statement.init(self);
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, return_value: Expression) !*ReturnStatement {
        const stmt = try allocator.create(ReturnStatement);
        stmt.* = .{
            .allocator = allocator,
            .token = token,
            .return_value = return_value,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return stmt;
    }

    pub fn deinit(self: *ReturnStatement) void {
        self.str_list.deinit();
        self.return_value.deinit();
        self.allocator.destroy(self);
    }
};

pub const ExpressionStatement = struct {
    allocator: std.mem.Allocator,
    token: Token,
    expression: Expression,

    fn statementNode(_: *ExpressionStatement) void {}

    pub fn nodeType(_: *ExpressionStatement) NodeType {
        return .ExpressionStatement;
    }

    fn tokenLiteral(self: *ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    fn deinit(self: *ExpressionStatement) void {
        self.expression.deinit();
        self.allocator.destroy(self);
    }

    fn string(self: *ExpressionStatement) ![]const u8 {
        return self.expression.string();
    }

    pub fn statement(self: *ExpressionStatement) Statement {
        return Statement.init(self);
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, expression: Expression) !*ExpressionStatement {
        const stmt = try allocator.create(ExpressionStatement);
        stmt.* = .{
            .allocator = allocator,
            .token = token,
            .expression = expression,
        };
        return stmt;
    }
};

pub const BlockStatement = struct {
    allocator: std.mem.Allocator,
    token: Token,
    statements: std.ArrayList(Statement),
    str_list: std.ArrayList(u8),

    fn statementNode(_: *BlockStatement) void {}

    pub fn nodeType(_: *BlockStatement) NodeType {
        return .BlockStatement;
    }

    pub fn tokenLiteral(self: *BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *BlockStatement) ![]const u8 {
        for (self.statements.items) |stmt| {
            try self.str_list.appendSlice(try stmt.string());
        }

        return self.str_list.items;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token) !*BlockStatement {
        const block = try allocator.create(BlockStatement);
        block.* = .{
            .allocator = allocator,
            .token = token,
            .statements = std.ArrayList(Statement).init(allocator),
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return block;
    }

    pub fn deinit(self: *BlockStatement) void {
        for (self.statements.items) |stmt| {
            stmt.deinit();
        }
        self.statements.deinit();
        self.str_list.deinit();
        self.allocator.destroy(self);
    }

    pub fn statement(self: *BlockStatement) Statement {
        return Statement.init(self);
    }
};

pub const PrefixExpression = struct {
    allocator: std.mem.Allocator,
    token: Token,
    operator: []const u8,
    right: Expression,
    str_list: std.ArrayList(u8),

    fn expressionNode(_: *PrefixExpression) void {}

    pub fn nodeType(_: *PrefixExpression) NodeType {
        return .PrefixExpression;
    }

    pub fn tokenLiteral(self: *PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *PrefixExpression) ![]const u8 {
        try self.str_list.appendSlice("(");
        try self.str_list.appendSlice(self.operator);
        try self.str_list.appendSlice(try self.right.string());
        try self.str_list.appendSlice(")");
        return self.str_list.items;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, operator: []const u8, right: Expression) !*PrefixExpression {
        const prefix_exp = try allocator.create(PrefixExpression);
        prefix_exp.* = .{
            .allocator = allocator,
            .token = token,
            .operator = operator,
            .right = right,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return prefix_exp;
    }

    pub fn deinit(self: *PrefixExpression) void {
        self.str_list.deinit();
        self.right.deinit();
        self.allocator.destroy(self);
    }

    pub fn expression(self: *PrefixExpression) Expression {
        return Expression.init(self);
    }
};

pub const InfixExpression = struct {
    allocator: std.mem.Allocator,
    token: Token,
    left: Expression,
    operator: []const u8,
    right: Expression,
    str_list: std.ArrayList(u8),

    fn expressionNode(_: *InfixExpression) void {}

    pub fn nodeType(_: *InfixExpression) NodeType {
        return .InfixExpression;
    }

    fn tokenLiteral(self: *InfixExpression) []const u8 {
        return self.token.literal;
    }

    fn string(self: *InfixExpression) ![]const u8 {
        try self.str_list.appendSlice("(");
        try self.str_list.appendSlice(try self.left.string());
        try self.str_list.appendSlice(" ");
        try self.str_list.appendSlice(self.operator);
        try self.str_list.appendSlice(" ");
        try self.str_list.appendSlice(try self.right.string());
        try self.str_list.appendSlice(")");
        return self.str_list.items;
    }

    pub fn expression(self: *InfixExpression) Expression {
        return Expression.init(self);
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, operator: []const u8, left: Expression, right: Expression) !*InfixExpression {
        const infix_exp = try allocator.create(InfixExpression);
        infix_exp.* = .{
            .allocator = allocator,
            .token = token,
            .left = left,
            .operator = operator,
            .right = right,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return infix_exp;
    }

    pub fn deinit(self: *InfixExpression) void {
        self.str_list.deinit();
        self.left.deinit();
        self.right.deinit();
        self.allocator.destroy(self);
    }
};

pub const IfExpression = struct {
    allocator: std.mem.Allocator,
    token: Token,
    condition: Expression,
    consequence: *BlockStatement,
    alternative: ?*BlockStatement,
    str_list: std.ArrayList(u8),

    fn expressionNode(_: *IfExpression) void {}

    pub fn nodeType(_: *IfExpression) NodeType {
        return .IfExpression;
    }

    fn tokenLiteral(self: *IfExpression) []const u8 {
        return self.token.literal;
    }

    fn string(self: *IfExpression) ![]const u8 {
        try self.str_list.appendSlice("if");
        try self.str_list.appendSlice(try self.condition.string());
        try self.str_list.appendSlice(" ");
        try self.str_list.appendSlice(try self.consequence.string());

        if (self.alternative) |alt| {
            try self.str_list.appendSlice("else ");
            try self.str_list.appendSlice(try alt.string());
        }

        return self.str_list.items;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, condition: Expression, consequence: *BlockStatement, alternative: ?*BlockStatement) !*IfExpression {
        const if_exp = try allocator.create(IfExpression);
        if_exp.* = .{
            .allocator = allocator,
            .token = token,
            .condition = condition,
            .consequence = consequence,
            .alternative = alternative,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return if_exp;
    }

    pub fn deinit(self: *IfExpression) void {
        self.str_list.deinit();
        self.condition.deinit();
        self.consequence.deinit();
        if (self.alternative) |alt| {
            alt.deinit();
        }
        self.allocator.destroy(self);
    }

    pub fn expression(self: *IfExpression) Expression {
        return Expression.init(self);
    }
};

pub const Identifier = struct {
    allocator: std.mem.Allocator,
    token: Token,
    value: []const u8,

    pub fn nodeType(_: *Identifier) NodeType {
        return .Identifier;
    }

    pub fn tokenLiteral(self: *Identifier) []const u8 {
        return self.token.literal;
    }

    fn expressionNode(_: *Identifier) void {}

    pub fn deinit(self: *Identifier) void {
        self.allocator.destroy(self);
    }

    fn string(self: *Identifier) ![]const u8 {
        return self.value;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, value: []const u8) !*Identifier {
        const ident = try allocator.create(Identifier);
        ident.* = .{
            .allocator = allocator,
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
    allocator: std.mem.Allocator,
    token: Token,
    value: i64,

    pub fn nodeType(_: *IntegerLiteral) NodeType {
        return .IntegerLiteral;
    }

    pub fn tokenLiteral(self: *IntegerLiteral) []const u8 {
        return self.token.literal;
    }

    fn expressionNode(_: *IntegerLiteral) void {}

    pub fn deinit(self: *IntegerLiteral) void {
        self.allocator.destroy(self);
    }

    pub fn string(self: *IntegerLiteral) ![]const u8 {
        return self.token.literal;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, value: i64) !*IntegerLiteral {
        const lit = try allocator.create(IntegerLiteral);
        lit.* = .{
            .allocator = allocator,
            .token = token,
            .value = value,
        };
        return lit;
    }

    pub fn expression(self: *IntegerLiteral) Expression {
        return Expression.init(self);
    }
};

pub const Boolean = struct {
    allocator: std.mem.Allocator,
    token: Token,
    value: bool,

    pub fn nodeType(_: *Boolean) NodeType {
        return .Boolean;
    }

    pub fn tokenLiteral(self: *Boolean) []const u8 {
        return self.token.literal;
    }

    fn expressionNode(_: *Boolean) void {}

    pub fn string(self: *Boolean) ![]const u8 {
        return self.token.literal;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, value: bool) !*Boolean {
        const b = try allocator.create(Boolean);
        b.* = .{
            .allocator = allocator,
            .token = token,
            .value = value,
        };
        return b;
    }

    pub fn deinit(self: *Boolean) void {
        self.allocator.destroy(self);
    }

    pub fn expression(self: *Boolean) Expression {
        return Expression.init(self);
    }
};

pub const StringLiteral = struct {
    allocator: std.mem.Allocator,
    token: Token,
    value: []const u8,

    pub fn nodeType(_: *StringLiteral) NodeType {
        return .StringLiteral;
    }

    pub fn tokenLiteral(self: *StringLiteral) []const u8 {
        return self.token.literal;
    }

    fn expressionNode(_: *StringLiteral) void {}

    pub fn string(self: *StringLiteral) ![]const u8 {
        return self.token.literal;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, value: []const u8) !*StringLiteral {
        const lit = try allocator.create(StringLiteral);
        lit.* = .{
            .allocator = allocator,
            .token = token,
            .value = value,
        };
        return lit;
    }

    pub fn deinit(self: *StringLiteral) void {
        self.allocator.destroy(self);
    }

    pub fn expression(self: *StringLiteral) Expression {
        return Expression.init(self);
    }
};

pub const FunctionLiteral = struct {
    allocator: std.mem.Allocator,
    token: Token,
    parameters: std.ArrayList(*Identifier),
    body: *BlockStatement,
    str_list: std.ArrayList(u8),

    fn expressionNode(_: *FunctionLiteral) void {}

    pub fn nodeType(_: *FunctionLiteral) NodeType {
        return .FunctionLiteral;
    }

    fn tokenLiteral(self: *FunctionLiteral) []const u8 {
        return self.token.literal;
    }

    fn string(self: *FunctionLiteral) ![]const u8 {
        try self.str_list.appendSlice(self.tokenLiteral());
        try self.str_list.appendSlice("(");
        for (self.parameters.items, 0..) |param, index| {
            if (index > 0) {
                try self.str_list.appendSlice(", ");
            }
            try self.str_list.appendSlice(try param.string());
        }
        try self.str_list.appendSlice(") ");
        try self.str_list.appendSlice(try self.body.string());
        return self.str_list.items;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, parameters: std.ArrayList(*Identifier), body: *BlockStatement) !*FunctionLiteral {
        const lit = try allocator.create(FunctionLiteral);
        lit.* = .{
            .allocator = allocator,
            .token = token,
            .parameters = parameters,
            .body = body,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return lit;
    }

    pub fn deinit(self: *FunctionLiteral) void {
        self.str_list.deinit();
        for (self.parameters.items) |param| {
            param.deinit();
        }
        self.parameters.deinit();
        self.body.deinit();
        self.allocator.destroy(self);
    }

    pub fn expression(self: *FunctionLiteral) Expression {
        return Expression.init(self);
    }
};

pub const CallExpression = struct {
    allocator: std.mem.Allocator,
    token: Token,
    function: Expression,
    arguments: std.ArrayList(Expression),
    str_list: std.ArrayList(u8),

    fn expressionNode(_: *CallExpression) void {}

    pub fn nodeType(_: *CallExpression) NodeType {
        return .CallExpression;
    }

    fn tokenLiteral(self: *CallExpression) []const u8 {
        return self.token.literal;
    }

    fn string(self: *CallExpression) ![]const u8 {
        try self.str_list.appendSlice(try self.function.string());
        try self.str_list.appendSlice("(");
        for (self.arguments.items, 0..) |arg, index| {
            if (index > 0) {
                try self.str_list.appendSlice(", ");
            }
            try self.str_list.appendSlice(try arg.string());
        }
        try self.str_list.appendSlice(")");

        return self.str_list.items;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, function: Expression, args: std.ArrayList(Expression)) !*CallExpression {
        const call_exp = try allocator.create(CallExpression);
        call_exp.* = .{
            .allocator = allocator,
            .token = token,
            .function = function,
            .arguments = args,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return call_exp;
    }

    fn deinit(self: *CallExpression) void {
        self.str_list.deinit();
        self.function.deinit();
        for (self.arguments.items) |arg| {
            arg.deinit();
        }
        self.arguments.deinit();
        self.allocator.destroy(self);
    }

    pub fn expression(self: *CallExpression) Expression {
        return Expression.init(self);
    }
};

pub const ArrayLiteral = struct {
    allocator: std.mem.Allocator,
    token: Token,
    elements: std.ArrayList(Expression),
    str_list: std.ArrayList(u8),

    fn expressionNode(_: *ArrayLiteral) void {}

    pub fn nodeType(_: *ArrayLiteral) NodeType {
        return .ArrayLiteral;
    }

    fn tokenLiteral(self: *ArrayLiteral) []const u8 {
        return self.token.literal;
    }

    fn string(self: *ArrayLiteral) ![]const u8 {
        try self.str_list.appendSlice("[");
        for (self.elements.items, 0..) |elem, index| {
            if (index > 0) {
                try self.str_list.appendSlice(", ");
            }
            try self.str_list.appendSlice(try elem.string());
        }
        try self.str_list.appendSlice("]");

        return self.str_list.items;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, elements: std.ArrayList(Expression)) !*ArrayLiteral {
        const array_lit = try allocator.create(ArrayLiteral);
        array_lit.* = .{
            .allocator = allocator,
            .token = token,
            .elements = elements,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return array_lit;
    }

    fn deinit(self: *ArrayLiteral) void {
        self.str_list.deinit();
        for (self.elements.items) |elem| {
            elem.deinit();
        }
        self.elements.deinit();
        self.allocator.destroy(self);
    }

    pub fn expression(self: *ArrayLiteral) Expression {
        return Expression.init(self);
    }
};

pub const IndexExpression = struct {
    allocator: std.mem.Allocator,
    token: Token,
    left: Expression,
    index: Expression,
    str_list: std.ArrayList(u8),

    fn expressionNode(_: *IndexExpression) void {}

    pub fn nodeType(_: *IndexExpression) NodeType {
        return .IndexExpression;
    }

    fn tokenLiteral(self: *IndexExpression) []const u8 {
        return self.token.literal;
    }

    fn string(self: *IndexExpression) ![]const u8 {
        try self.str_list.appendSlice("(");
        try self.str_list.appendSlice(try self.left.string());
        try self.str_list.appendSlice("[");
        try self.str_list.appendSlice(try self.index.string());
        try self.str_list.appendSlice("])");

        return self.str_list.items;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token, left: Expression, index: Expression) !*IndexExpression {
        const index_exp = try allocator.create(IndexExpression);
        index_exp.* = .{
            .allocator = allocator,
            .token = token,
            .left = left,
            .index = index,
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return index_exp;
    }

    fn deinit(self: *IndexExpression) void {
        self.str_list.deinit();
        self.left.deinit();
        self.index.deinit();
        self.allocator.destroy(self);
    }

    pub fn expression(self: *IndexExpression) Expression {
        return Expression.init(self);
    }
};

pub const HashLiteral = struct {
    allocator: std.mem.Allocator,
    token: Token,
    pairs: std.AutoHashMap(Expression, Expression),
    str_list: std.ArrayList(u8),

    fn expressionNode(_: *HashLiteral) void {}

    pub fn nodeType(_: *HashLiteral) NodeType {
        return .HashLiteral;
    }

    fn tokenLiteral(self: *HashLiteral) []const u8 {
        return self.token.literal;
    }

    fn string(self: *HashLiteral) ![]const u8 {
        try self.str_list.appendSlice("{");
        var it = self.pairs.iterator();
        var first = true;
        while (it.next()) |pair| {
            if (!first) {
                try self.str_list.appendSlice(", ");
            } else {
                first = false;
            }
            const key_str = try pair.key_ptr.*.string();
            const value_str = try pair.value_ptr.*.string();
            try self.str_list.appendSlice(key_str);
            try self.str_list.appendSlice(": ");
            try self.str_list.appendSlice(value_str);
        }
        try self.str_list.appendSlice("}");

        return self.str_list.items;
    }

    pub fn init(allocator: std.mem.Allocator, token: Token) !*HashLiteral {
        const hash_lit = try allocator.create(HashLiteral);
        hash_lit.* = .{
            .allocator = allocator,
            .token = token,
            .pairs = std.AutoHashMap(Expression, Expression).init(allocator),
            .str_list = std.ArrayList(u8).init(allocator),
        };
        return hash_lit;
    }

    fn deinit(self: *HashLiteral) void {
        self.str_list.deinit();
        var it = self.pairs.iterator();
        while (it.next()) |pair| {
            pair.key_ptr.deinit();
            pair.value_ptr.deinit();
        }
        self.pairs.deinit();
        self.allocator.destroy(self);
    }

    pub fn expression(self: *HashLiteral) Expression {
        return Expression.init(self);
    }
};

test "test string" {
    const allocator = testing.allocator;

    const program = try Program.init(allocator);
    defer program.deinit();

    const name = try Identifier.init(allocator, Token{ .type = .Ident, .literal = "myVar" }, "myVar");
    const value = try Identifier.init(allocator, Token{ .type = .Ident, .literal = "anotherVar" }, "anotherVar");
    const let_stmt = try LetStatement.init(allocator, Token{ .type = .Let, .literal = "let" }, name, value.expression());
    try program.statements.append(let_stmt.statement());

    const out = try program.string();
    try testing.expect(std.mem.eql(u8, out, "let myVar = anotherVar;"));
}
