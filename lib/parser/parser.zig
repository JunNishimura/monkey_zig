const std = @import("std");
const testing = std.testing;
const lexer = @import("lexer");
const tok = @import("token");
const ast = @import("ast");

const PrefixParseFn = *const fn (*Parser, std.mem.Allocator) anyerror!ast.Expression;
const InfixParseFn = *const fn (*Parser, std.mem.Allocator, ast.Expression) anyerror!ast.Expression;

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

    pub fn is_lower(self: Precedence, other: Precedence) bool {
        return self.rank() < other.rank();
    }
};

const Precedences = std.StaticStringMap(Precedence).initComptime(.{
    .{ tok.TokenType.Eq.string(), Precedence.Equals },
    .{ tok.TokenType.NotEq.string(), Precedence.Equals },
    .{ tok.TokenType.LessThan.string(), Precedence.LessGreater },
    .{ tok.TokenType.GreaterThan.string(), Precedence.LessGreater },
    .{ tok.TokenType.Plus.string(), Precedence.Sum },
    .{ tok.TokenType.Minus.string(), Precedence.Sum },
    .{ tok.TokenType.Asterisk.string(), Precedence.Product },
    .{ tok.TokenType.Slash.string(), Precedence.Product },
});

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
        try p.prefix_parse_fns.put(.Int, parse_integer_literal);
        try p.prefix_parse_fns.put(.Bang, parse_prefix_expression);
        try p.prefix_parse_fns.put(.Minus, parse_prefix_expression);
        try p.infix_parse_fns.put(.Plus, parse_infix_expression);
        try p.infix_parse_fns.put(.Minus, parse_infix_expression);
        try p.infix_parse_fns.put(.Asterisk, parse_infix_expression);
        try p.infix_parse_fns.put(.Slash, parse_infix_expression);
        try p.infix_parse_fns.put(.Eq, parse_infix_expression);
        try p.infix_parse_fns.put(.NotEq, parse_infix_expression);
        try p.infix_parse_fns.put(.LessThan, parse_infix_expression);
        try p.infix_parse_fns.put(.GreaterThan, parse_infix_expression);

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

    fn peek_precedence(self: *Parser) Precedence {
        const precedence = Precedences.get(self.peek_token.type.string());
        return precedence orelse Precedence.Lowest;
    }

    fn cur_precedence(self: *Parser) Precedence {
        const precedence = Precedences.get(self.cur_token.type.string());
        return precedence orelse Precedence.Lowest;
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

    fn parse_expression(self: *Parser, allocator: std.mem.Allocator, precedence: Precedence) !?ast.Expression {
        const prefix = self.prefix_parse_fns.get(self.cur_token.type);
        if (prefix == null) {
            try self.no_prefix_parse_fn_error(allocator, self.cur_token.type);
            return null;
        }

        var left_exp = try prefix.?(self, allocator);

        while (!self.peek_token_is(.Semicolon) and precedence.is_lower(self.peek_precedence())) {
            const infix = self.infix_parse_fns.get(self.peek_token.type);
            if (infix == null) {
                return left_exp;
            }

            self.next_token();

            left_exp = try infix.?(self, allocator, left_exp);
        }

        return left_exp;
    }

    fn parse_expression_statement(self: *Parser, allocator: std.mem.Allocator) !*ast.ExpressionStatement {
        const exp_stmt = try ast.ExpressionStatement.init(allocator, self.cur_token);

        exp_stmt.expression = try self.parse_expression(allocator, .Lowest);

        if (self.peek_token_is(.Semicolon)) {
            self.next_token();
        }

        return exp_stmt;
    }

    fn parse_integer_literal(self: *Parser, allocator: std.mem.Allocator) !ast.Expression {
        const lit = try ast.IntegerLiteral.init(allocator, self.cur_token);

        const int_value = try std.fmt.parseInt(i64, self.cur_token.literal, 10);
        lit.value = int_value;

        return lit.expression();
    }

    fn parse_prefix_expression(self: *Parser, allocator: std.mem.Allocator) !ast.Expression {
        const prefix_exp = try ast.PrefixExpression.init(allocator, self.cur_token, self.cur_token.literal);

        self.next_token();

        prefix_exp.right = try self.parse_expression(allocator, .Prefix);

        return prefix_exp.expression();
    }

    fn parse_infix_expression(self: *Parser, allocator: std.mem.Allocator, left: ast.Expression) !ast.Expression {
        const infix_exp = try ast.InfixExpression.init(
            allocator,
            self.cur_token,
            self.cur_token.literal,
            left,
        );

        const precedence = self.cur_precedence();
        self.next_token();
        infix_exp.right = try self.parse_expression(allocator, precedence);

        return infix_exp.expression();
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

    fn no_prefix_parse_fn_error(self: *Parser, allocator: std.mem.Allocator, token_type: tok.TokenType) !void {
        const msg = try std.fmt.allocPrint(
            allocator,
            "no prefix parse function for {any} found",
            .{token_type},
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

test "test integer literal expression" {
    const input =
        \\5;
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

    const lit: *ast.IntegerLiteral = @ptrCast(@alignCast(exp_stmt.expression.?.ptr));
    try testing.expect(std.mem.eql(u8, lit.token_literal(), "5"));
    try testing.expect(lit.value == 5);
}

fn test_integer_literal(allocator: std.mem.Allocator, il: ast.Expression, value: i64) !bool {
    const integ: *ast.IntegerLiteral = @ptrCast(@alignCast(il.ptr));

    if (integ.value != value) {
        return false;
    }

    const integ_str = try std.fmt.allocPrint(allocator, "{d}", .{value});
    defer allocator.free(integ_str);

    if (!std.mem.eql(u8, integ.token_literal(), integ_str)) {
        return false;
    }

    return true;
}

test "test parsing prefix expressions" {
    const prefix_tests = [_]struct {
        input: []const u8,
        operator: []const u8,
        integer_value: i64,
    }{
        .{ .input = "!5", .operator = "!", .integer_value = 5 },
        .{ .input = "-15", .operator = "-", .integer_value = 15 },
    };

    for (prefix_tests) |prefix_test| {
        const allocator = std.testing.allocator;

        const l = try lexer.Lexer.init(allocator, prefix_test.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit(allocator);

        const program = try p.parse_program(allocator);
        check_parser_errors(p);
        defer program.deinit(allocator);

        try testing.expect(program.statements.items.len == 1);

        const stmt = program.statements.items[0];
        const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.ptr));

        const prefix_expr: *ast.PrefixExpression = @ptrCast(@alignCast(exp_stmt.expression.?.ptr));
        try testing.expect(std.mem.eql(u8, prefix_expr.operator, prefix_test.operator));
        try testing.expect(try test_integer_literal(allocator, prefix_expr.right.?, prefix_test.integer_value));
    }
}

test "test parsing infix expressions" {
    const infix_tests = [_]struct {
        input: []const u8,
        left_value: i64,
        operator: []const u8,
        right_value: i64,
    }{
        .{ .input = "5 + 5;", .left_value = 5, .operator = "+", .right_value = 5 },
        .{ .input = "5 - 5;", .left_value = 5, .operator = "-", .right_value = 5 },
        .{ .input = "5 * 5;", .left_value = 5, .operator = "*", .right_value = 5 },
        .{ .input = "5 / 5;", .left_value = 5, .operator = "/", .right_value = 5 },
        .{ .input = "5 > 5;", .left_value = 5, .operator = ">", .right_value = 5 },
        .{ .input = "5 < 5;", .left_value = 5, .operator = "<", .right_value = 5 },
        .{ .input = "5 == 5;", .left_value = 5, .operator = "==", .right_value = 5 },
        .{ .input = "5 != 5;", .left_value = 5, .operator = "!=", .right_value = 5 },
    };

    for (infix_tests) |infix_test| {
        const allocator = std.testing.allocator;

        const l = try lexer.Lexer.init(allocator, infix_test.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit(allocator);

        const program = try p.parse_program(allocator);
        check_parser_errors(p);
        defer program.deinit(allocator);

        try testing.expect(program.statements.items.len == 1);

        const stmt = program.statements.items[0];
        const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.ptr));

        const infix_expr: *ast.InfixExpression = @ptrCast(@alignCast(exp_stmt.expression.?.ptr));
        try testing.expect(try test_integer_literal(allocator, infix_expr.left, infix_test.left_value));
        try testing.expect(std.mem.eql(u8, infix_expr.operator, infix_test.operator));
        try testing.expect(try test_integer_literal(allocator, infix_expr.right.?, infix_test.right_value));
    }
}

test "test operator precedence parsing" {
    const precedence_tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{ .input = "-a * b", .expected = "((-a) * b)" },
        .{ .input = "!-a", .expected = "(!(-a))" },
        .{ .input = "a + b + c", .expected = "((a + b) + c)" },
        .{ .input = "a + b - c", .expected = "((a + b) - c)" },
        .{ .input = "a * b * c", .expected = "((a * b) * c)" },
        .{ .input = "a * b / c", .expected = "((a * b) / c)" },
        .{ .input = "a + b / c", .expected = "(a + (b / c))" },
        .{ .input = "a + b * c + d / e - f", .expected = "(((a + (b * c)) + (d / e)) - f)" },
        .{ .input = "3 + 4; -5 * 5", .expected = "(3 + 4)((-5) * 5)" },
        .{ .input = "5 > 4 == 3 < 4", .expected = "((5 > 4) == (3 < 4))" },
        .{ .input = "5 < 4 != 3 > 4", .expected = "((5 < 4) != (3 > 4))" },
        .{ .input = "3 + 4 * 5 == 3 * 1 + 4 * 5", .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
    };

    for (precedence_tests) |precedence_test| {
        const allocator = std.testing.allocator;

        const l = try lexer.Lexer.init(allocator, precedence_test.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit(allocator);

        const program = try p.parse_program(allocator);
        check_parser_errors(p);
        defer program.deinit(allocator);

        const program_str = try program.string();
        try testing.expect(std.mem.eql(u8, program_str, precedence_test.expected));
    }
}

fn test_identifier(exp: ast.Expression, value: []const u8) !bool {
    const ident: *ast.Identifier = @ptrCast(@alignCast(exp.ptr));

    try testing.expect(std.mem.eql(u8, ident.value, value));
    try testing.expect(std.mem.eql(u8, ident.token_literal(), value));
}

fn test_literal_expression(allocator: std.mem.Allocator, exp: ast.Expression, expected: anytype) !bool {
    switch (@TypeOf(expected)) {
        i64 => return test_integer_literal(allocator, exp, expected),
        []const u8 => return test_identifier(exp, expected),
        else => return false,
    }
}

fn test_infix_expressoin(allocator: std.mem.Allocator, exp: ast.Expression, left: anytype, operator: []const u8, right: anytype) !bool {
    const infix_exp: *ast.InfixExpression = @ptrCast(@alignCast(exp.ptr));

    if (!(try test_literal_expression(allocator, infix_exp.left, left))) {
        return false;
    }

    if (!(try std.mem.eql(u8, infix_exp.operator, operator))) {
        return false;
    }

    if (!(try test_literal_expression(allocator, infix_exp.right, right))) {
        return false;
    }

    return true;
}
