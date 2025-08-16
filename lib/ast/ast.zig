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
    node: Node,

    pub fn statement_node(self: Statement) void {
        self.statement_node_fn(self.ptr);
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
    statements: []Statement,

    fn token_literal(self: *Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].token_literal();
        }
        return "";
    }
};

pub const LetStatement = struct {
    token: Token,
    name: *Identifier,
    value: Expression,

    fn token_literal(self: LetStatement) []const u8 {
        return self.token.literal;
    }
    fn statement_node(_: LetStatement) void {}

    fn statement(self: *LetStatement) Statement {
        return .{
            .ptr = self,
            .statement_node_fn = statement_node,
            .node = .{ .ptr = self, .token_literal_fn = token_literal },
        };
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn token_literal(self: *Identifier) []const u8 {
        return self.token.literal;
    }
    fn expression_node(_: *Identifier) void {}
};
