const std = @import("std");
const testing = std.testing;
const lexer = @import("lexer");
const tok = @import("token");
const ast = @import("ast");

const PrefixParseFn = *const fn (*Parser, std.mem.Allocator) anyerror!ast.Expression;
const InfixParseFn = *const fn (*Parser, std.mem.Allocator, ast.Expression) ast.Expression;

const Precedence = enum {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,

    fn rank(self: Precedence) u8 {
        switch (self) {
            .Lowest => return 1,
            .Equals => return 2,
            .LessGreater => return 3,
            .Sum => return 4,
            .Product => return 5,
            .Prefix => return 6,
            .Call => return 7,
        }
    }
};

const Parser = struct {
    l: *lexer.Lexer,
    cur_token: tok.Token,
    peek_token: tok.Token,
    errors: std.ArrayList([]u8),
    prefix_parse_fns: std.AutoHashMap(tok.TokenType, PrefixParseFn),
    infix_parse_fns: std.AutoHashMap(tok.TokenType, InfixParseFn),

    fn init(allocator: std.mem.Allocator, l: *lexer.Lexer) !*Parser {
        var p = try allocator.create(Parser);
        p.* = .{
            .l = l,
            .cur_token = .{ .type = .Illegal, .literal = "" },
            .peek_token = .{ .type = .Illegal, .literal = "" },
            .errors = std.ArrayList([]u8).init(allocator),
            .prefix_parse_fns = std.AutoHashMap(tok.TokenType, PrefixParseFn).init(allocator),
            .infix_parse_fns = std.AutoHashMap(tok.TokenType, InfixParseFn).init(allocator),
        };
        try p.prefix_parse_fns.put(.Ident, parse_identifier);

        p.next_token();
        p.next_token();
        return p;
    }

    fn deinit(self: *Parser, allocator: std.mem.Allocator) void {
        for (self.errors.items) |err| {
            allocator.free(err);
        }
        self.errors.deinit();
        self.prefix_parse_fns.deinit();
        self.infix_parse_fns.deinit();
        allocator.destroy(self);
    }

    fn next_token(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.l.next_token();
    }

    fn peek_token_is(self: *Parser, token_type: tok.TokenType) bool {
        return self.peek_token.type == token_type;
    }

    fn expect_peek(self: *Parser, allocator: std.mem.Allocator, token_type: tok.TokenType) !bool {
        if (self.peek_token_is(token_type)) {
            self.next_token();
            return true;
        }
        try self.peek_error(allocator, token_type);
        return false;
    }

    fn cur_token_is(self: *Parser, token_type: tok.TokenType) bool {
        return self.cur_token.type == token_type;
    }

    fn parse_let_statement(self: *Parser, allocator: std.mem.Allocator) !?*ast.LetStatement {
        const let_stmt = try ast.LetStatement.init(allocator, self.cur_token);

        const is_ident = try self.expect_peek(allocator, .Ident);
        if (!is_ident) {
            return null;
        }

        let_stmt.name = try ast.Identifier.init(allocator, self.cur_token, self.cur_token.literal);

        const is_assign = try self.expect_peek(allocator, .Assign);
        if (!is_assign) {
            return null;
        }

        while (!self.cur_token_is(.Semicolon)) {
            self.next_token();
        }

        return let_stmt;
    }

    fn parse_identifier(self: *Parser, allocator: std.mem.Allocator) !ast.Expression {
        return (try ast.Identifier.init(allocator, self.cur_token, self.cur_token.literal)).expression();
    }

    fn parse_return_statement(self: *Parser, allocator: std.mem.Allocator) !*ast.ReturnStatement {
        const return_stmt = try ast.ReturnStatement.init(allocator, self.cur_token);

        self.next_token();

        while (!self.cur_token_is(.Semicolon)) {
            self.next_token();
        }

        return return_stmt;
    }

    fn parse_expression(self: *Parser, allocator: std.mem.Allocator, _: Precedence) !?ast.Expression {
        const prefix = self.prefix_parse_fns.get(self.cur_token.type);
        if (prefix) |prefix_fn| {
            const left_exp = try prefix_fn(self, allocator);

            return left_exp;
        }

        return null;
    }

    fn parse_expression_statement(self: *Parser, allocator: std.mem.Allocator) !*ast.ExpressionStatement {
        const exp_stmt = try ast.ExpressionStatement.init(allocator, self.cur_token);

        exp_stmt.expression = try self.parse_expression(allocator, Precedence.Lowest);

        if (self.peek_token_is(.Semicolon)) {
            self.next_token();
        }

        return exp_stmt;
    }

    fn parse_statement(self: *Parser, allocator: std.mem.Allocator) !?ast.Statement {
        switch (self.cur_token.type) {
            .Let => {
                const let_stmt = try self.parse_let_statement(allocator);
                if (let_stmt) |ls| {
                    return ls.statement();
                }
                return null;
            },
            .Return => {
                const return_stmt = try self.parse_return_statement(allocator);
                return return_stmt.statement();
            },
            else => {
                const exp_stmt = try self.parse_expression_statement(allocator);
                return exp_stmt.statement();
            },
        }
    }

    fn parse_program(self: *Parser, allocator: std.mem.Allocator) !*ast.Program {
        const program = try ast.Program.init(allocator);

        while (self.cur_token.type != .Eof) {
            const stmt = try self.parse_statement(allocator);
            if (stmt) |s| {
                try program.statements.append(s);
            }
            self.next_token();
        }

        return program;
    }

    fn peek_error(self: *Parser, allocator: std.mem.Allocator, token_type: tok.TokenType) !void {
        const msg = try std.fmt.allocPrint(
            allocator,
            "expected next token to be {any}, got {any} instead",
            .{
                token_type,
                self.peek_token.type,
            },
        );
        try self.errors.append(msg);
    }

    fn register_prefix(self: *Parser, token_type: tok.TokenType, prefix_fn: PrefixParseFn) !void {
        try self.prefix_parse_fns.put(token_type, prefix_fn);
    }

    fn register_infix(self: *Parser, token_type: tok.TokenType, infix_fn: InfixParseFn) !void {
        try self.infix_parse_fns.put(token_type, infix_fn);
    }
};

test "test let statements" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    const allocator = std.testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit(allocator);

    const program = try p.parse_program(allocator);
    check_parser_errors(p);
    defer program.deinit(allocator);

    try testing.expect(program.statements.items.len == 3);

    const expected = [_]struct {
        identifier: []const u8,
    }{
        .{ .identifier = "x" },
        .{ .identifier = "y" },
        .{ .identifier = "foobar" },
    };

    for (program.statements.items, expected) |stmt, exp| {
        try testing.expect(test_let_statement(stmt, exp.identifier));
    }
}

fn check_parser_errors(parser: *Parser) void {
    const errors = parser.errors;
    if (errors.items.len == 0) {
        return;
    }

    std.log.err("parser has {d} errors:", .{errors.items.len});
    for (errors.items) |err| {
        std.log.err("parser error: {s}", .{err});
    }

    // exit process
    std.process.exit(1);
}

fn test_let_statement(s: ast.Statement, name: []const u8) bool {
    if (!std.mem.eql(u8, s.token_literal(), "let")) {
        return false;
    }

    const let_stmt: *ast.LetStatement = @ptrCast(@alignCast(s.ptr));

    if (let_stmt.name == null) {
        return false;
    }
    if (!std.mem.eql(u8, let_stmt.name.?.value, name)) {
        return false;
    }
    if (!std.mem.eql(u8, let_stmt.name.?.token_literal(), name)) {
        return false;
    }
    return true;
}

test "test return statement" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    const allocator = std.testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit(allocator);

    const program = try p.parse_program(allocator);
    check_parser_errors(p);
    defer program.deinit(allocator);

    try testing.expect(program.statements.items.len == 3);

    for (program.statements.items) |stmt| {
        const ret_stmt: *ast.ReturnStatement = @ptrCast(@alignCast(stmt.ptr));
        try testing.expect(std.mem.eql(u8, ret_stmt.token_literal(), "return"));
    }
}

test "test identifier expression" {
    const input =
        \\foobar;
    ;

    const allocator = std.testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit(allocator);

    const program = try p.parse_program(allocator);
    check_parser_errors(p);
    defer program.deinit(allocator);

    try testing.expect(program.statements.items.len == 1);

    const stmt = program.statements.items[0];
    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.ptr));

    const ident: *ast.Identifier = @ptrCast(@alignCast(exp_stmt.expression.?.ptr));
    try testing.expect(std.mem.eql(u8, ident.token_literal(), "foobar"));
    try testing.expect(std.mem.eql(u8, ident.value, "foobar"));
}
