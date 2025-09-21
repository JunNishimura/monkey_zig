const std = @import("std");
const testing = std.testing;
const lexer = @import("lexer");
const tok = @import("token");
const ast = @import("ast");

const PrefixParseFn = *const fn (*Parser) anyerror!?ast.Expression;
const InfixParseFn = *const fn (*Parser, ast.Expression) anyerror!?ast.Expression;

const Precedence = enum {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,

    fn rank(self: Precedence) u8 {
        switch (self) {
            .Lowest => return 1,
            .Equals => return 2,
            .LessGreater => return 3,
            .Sum => return 4,
            .Product => return 5,
            .Prefix => return 6,
            .Call => return 7,
            .Index => return 8,
        }
    }

    pub fn isLower(self: Precedence, other: Precedence) bool {
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
    .{ tok.TokenType.LParen.string(), Precedence.Call },
    .{ tok.TokenType.LBracket.string(), Precedence.Index },
});

pub const Parser = struct {
    allocator: std.mem.Allocator,
    l: *lexer.Lexer,
    cur_token: tok.Token,
    peek_token: tok.Token,
    errors: std.ArrayList([]u8),
    prefix_parse_fns: std.AutoHashMap(tok.TokenType, PrefixParseFn),
    infix_parse_fns: std.AutoHashMap(tok.TokenType, InfixParseFn),

    pub fn init(allocator: std.mem.Allocator, l: *lexer.Lexer) !*Parser {
        var p = try allocator.create(Parser);
        p.* = .{
            .allocator = allocator,
            .l = l,
            .cur_token = .{ .type = .Illegal, .literal = "" },
            .peek_token = .{ .type = .Illegal, .literal = "" },
            .errors = std.ArrayList([]u8).init(allocator),
            .prefix_parse_fns = std.AutoHashMap(tok.TokenType, PrefixParseFn).init(allocator),
            .infix_parse_fns = std.AutoHashMap(tok.TokenType, InfixParseFn).init(allocator),
        };
        try p.registerPrefix(.Ident, parseIdentifier);
        try p.registerPrefix(.Int, parseIntegerLiteral);
        try p.registerPrefix(.String, parseStringLiteral);
        try p.registerPrefix(.True, parseBoolean);
        try p.registerPrefix(.False, parseBoolean);
        try p.registerPrefix(.Bang, parsePrefixExpression);
        try p.registerPrefix(.Minus, parsePrefixExpression);
        try p.registerPrefix(.LParen, parseGroupExpression);
        try p.registerPrefix(.If, parseIfExpression);
        try p.registerPrefix(.Function, parseFunctionLiteral);
        try p.registerPrefix(.LBracket, parseArrayLiteral);
        try p.registerPrefix(.LBrace, parseHashLiteral);
        try p.registerInfix(.Plus, parseInfixExpression);
        try p.registerInfix(.Minus, parseInfixExpression);
        try p.registerInfix(.Asterisk, parseInfixExpression);
        try p.registerInfix(.Slash, parseInfixExpression);
        try p.registerInfix(.Eq, parseInfixExpression);
        try p.registerInfix(.NotEq, parseInfixExpression);
        try p.registerInfix(.LessThan, parseInfixExpression);
        try p.registerInfix(.GreaterThan, parseInfixExpression);
        try p.registerInfix(.LParen, parseCallExpression);
        try p.registerInfix(.LBracket, parseIndexExpression);

        p.nextToken();
        p.nextToken();
        return p;
    }

    pub fn deinit(self: *Parser) void {
        for (self.errors.items) |err| {
            self.allocator.free(err);
        }
        self.errors.deinit();
        self.prefix_parse_fns.deinit();
        self.infix_parse_fns.deinit();
        self.allocator.destroy(self);
    }

    fn nextToken(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.l.nextToken();
    }

    fn peekTokenIs(self: *Parser, token_type: tok.TokenType) bool {
        return self.peek_token.type == token_type;
    }

    fn expectPeek(self: *Parser, token_type: tok.TokenType) !bool {
        if (self.peekTokenIs(token_type)) {
            self.nextToken();
            return true;
        }
        try self.peekError(token_type);
        return false;
    }

    fn curTokenIs(self: *Parser, token_type: tok.TokenType) bool {
        return self.cur_token.type == token_type;
    }

    fn peekPrecedence(self: *Parser) Precedence {
        const precedence = Precedences.get(self.peek_token.type.string());
        return precedence orelse Precedence.Lowest;
    }

    fn curPrecedence(self: *Parser) Precedence {
        const precedence = Precedences.get(self.cur_token.type.string());
        return precedence orelse Precedence.Lowest;
    }

    fn parseLetStatement(self: *Parser) !?*ast.LetStatement {
        const let_cur_token = self.cur_token;

        if (!(try self.expectPeek(.Ident))) {
            return null;
        }

        const name = try ast.Identifier.init(self.allocator, self.cur_token, self.cur_token.literal);

        if (!(try self.expectPeek(.Assign))) {
            name.deinit();
            return null;
        }

        self.nextToken();

        const value = try self.parseExpression(.Lowest);

        if (self.peekTokenIs(.Semicolon)) {
            self.nextToken();
        }

        return try ast.LetStatement.init(self.allocator, let_cur_token, name, value.?);
    }

    fn parseIdentifier(self: *Parser) !?ast.Expression {
        return (try ast.Identifier.init(self.allocator, self.cur_token, self.cur_token.literal)).expression();
    }

    fn parseReturnStatement(self: *Parser) !*ast.ReturnStatement {
        const return_cur_token = self.cur_token;

        self.nextToken();

        const return_value = try self.parseExpression(.Lowest);

        if (self.peekTokenIs(.Semicolon)) {
            self.nextToken();
        }

        return try ast.ReturnStatement.init(self.allocator, return_cur_token, return_value.?);
    }

    fn parseBlockStatement(self: *Parser) !*ast.BlockStatement {
        const block_stmt = try ast.BlockStatement.init(self.allocator, self.cur_token);

        self.nextToken();

        while (!self.curTokenIs(.RBrace) and !self.curTokenIs(.Eof)) {
            if (try self.parseStatement()) |s| {
                try block_stmt.statements.append(s);
            }

            self.nextToken();
        }

        return block_stmt;
    }

    fn parseExpression(self: *Parser, precedence: Precedence) !?ast.Expression {
        const prefix = self.prefix_parse_fns.get(self.cur_token.type);
        if (prefix == null) {
            try self.noPrefixParseFnError(self.cur_token.type);
            return null;
        }

        var left_exp = try prefix.?(self);

        while (!self.peekTokenIs(.Semicolon) and precedence.isLower(self.peekPrecedence())) {
            const infix = self.infix_parse_fns.get(self.peek_token.type);
            if (infix == null) {
                return left_exp;
            }

            self.nextToken();

            left_exp = try infix.?(self, left_exp.?);
        }

        return left_exp;
    }

    fn parseExpressionStatement(self: *Parser) !*ast.ExpressionStatement {
        const cur_token = self.cur_token;

        const expression = try self.parseExpression(.Lowest);

        if (self.peekTokenIs(.Semicolon)) {
            self.nextToken();
        }

        return try ast.ExpressionStatement.init(self.allocator, cur_token, expression.?);
    }

    fn parseIntegerLiteral(self: *Parser) !?ast.Expression {
        const cur_token = self.cur_token;

        const int_value = try std.fmt.parseInt(i64, self.cur_token.literal, 10);

        return (try ast.IntegerLiteral.init(self.allocator, cur_token, int_value)).expression();
    }

    fn parseStringLiteral(self: *Parser) !?ast.Expression {
        return (try ast.StringLiteral.init(self.allocator, self.cur_token, self.cur_token.literal)).expression();
    }

    fn parseBoolean(self: *Parser) !?ast.Expression {
        return (try ast.Boolean.init(self.allocator, self.cur_token, self.curTokenIs(.True))).expression();
    }

    fn parsePrefixExpression(self: *Parser) !?ast.Expression {
        const cur_token = self.cur_token;

        self.nextToken();

        const right = try self.parseExpression(.Prefix);

        return (try ast.PrefixExpression.init(self.allocator, cur_token, cur_token.literal, right.?)).expression();
    }

    fn parseGroupExpression(self: *Parser) !?ast.Expression {
        self.nextToken();

        const exp = try self.parseExpression(.Lowest);

        if (!(try self.expectPeek(.RParen))) {
            return null;
        }

        return exp;
    }

    fn parseIfExpression(self: *Parser) !?ast.Expression {
        const if_cur_token = self.cur_token;

        if (!(try self.expectPeek(.LParen))) {
            return null;
        }

        self.nextToken();
        const condition = try self.parseExpression(.Lowest);

        if (!(try self.expectPeek(.RParen))) {
            return null;
        }

        if (!(try self.expectPeek(.LBrace))) {
            return null;
        }

        const consequence = try self.parseBlockStatement();

        var alternative: ?*ast.BlockStatement = null;
        if (self.peekTokenIs(.Else)) {
            self.nextToken();

            if (!(try self.expectPeek(.LBrace))) {
                return null;
            }

            alternative = try self.parseBlockStatement();
        }

        return (try ast.IfExpression.init(
            self.allocator,
            if_cur_token,
            condition.?,
            consequence,
            alternative,
        )).expression();
    }

    fn parseFunctionParameters(self: *Parser) !?std.ArrayList(*ast.Identifier) {
        var params = std.ArrayList(*ast.Identifier).init(self.allocator);

        if (self.peekTokenIs(.RParen)) {
            self.nextToken();
            return params;
        }

        self.nextToken();

        const param = try ast.Identifier.init(self.allocator, self.cur_token, self.cur_token.literal);
        try params.append(param);

        while (self.peekTokenIs(.Comma)) {
            self.nextToken();
            self.nextToken();

            const other_param = try ast.Identifier.init(self.allocator, self.cur_token, self.cur_token.literal);
            try params.append(other_param);
        }

        if (!(try self.expectPeek(.RParen))) {
            return null;
        }

        return params;
    }

    fn parseFunctionLiteral(self: *Parser) !?ast.Expression {
        const func_cur_token = self.cur_token;

        if (!(try self.expectPeek(.LParen))) {
            return null;
        }

        const params = try self.parseFunctionParameters();
        if (params == null) {
            return null;
        }

        if (!(try self.expectPeek(.LBrace))) {
            return null;
        }

        const body = try self.parseBlockStatement();

        return (try ast.FunctionLiteral.init(
            self.allocator,
            func_cur_token,
            params.?,
            body,
        )).expression();
    }

    fn parseArrayLiteral(self: *Parser) !?ast.Expression {
        const array_cur_token = self.cur_token;

        const elements = try self.parseExpressionList(.RBracket);
        if (elements == null) {
            return null;
        }

        return (try ast.ArrayLiteral.init(
            self.allocator,
            array_cur_token,
            elements.?,
        )).expression();
    }

    fn parseExpressionList(self: *Parser, end: tok.TokenType) !?std.ArrayList(ast.Expression) {
        var list = std.ArrayList(ast.Expression).init(self.allocator);

        if (self.peekTokenIs(end)) {
            self.nextToken();
            return list;
        }

        self.nextToken();
        const exp = try self.parseExpression(.Lowest);
        if (exp) |e| {
            try list.append(e);
        } else {
            return null;
        }

        while (self.peekTokenIs(.Comma)) {
            self.nextToken();
            self.nextToken();
            const other_exp = try self.parseExpression(.Lowest);
            if (other_exp) |e| {
                try list.append(e);
            } else {
                return null;
            }
        }

        if (!(try self.expectPeek(end))) {
            return null;
        }

        return list;
    }

    fn parseHashLiteral(self: *Parser) !?ast.Expression {
        const hash_lit = try ast.HashLiteral.init(self.allocator, self.cur_token);

        while (!self.peekTokenIs(.RBrace)) {
            self.nextToken();
            const key = try self.parseExpression(.Lowest);
            if (key == null) {
                return null;
            }

            if (!(try self.expectPeek(.Colon))) {
                return null;
            }

            self.nextToken();
            const value = try self.parseExpression(.Lowest);
            if (value == null) {
                return null;
            }

            try hash_lit.pairs.put(key.?, value.?);

            if (!self.peekTokenIs(.RBrace) and !(try self.expectPeek(.Comma))) {
                return null;
            }
        }

        if (!(try self.expectPeek(.RBrace))) {
            return null;
        }

        return hash_lit.expression();
    }

    fn parseInfixExpression(self: *Parser, left: ast.Expression) !?ast.Expression {
        const infix_cur_token = self.cur_token;

        const precedence = self.curPrecedence();
        self.nextToken();
        const right = try self.parseExpression(precedence);

        return (try ast.InfixExpression.init(
            self.allocator,
            infix_cur_token,
            infix_cur_token.literal,
            left,
            right.?,
        )).expression();
    }

    fn parseCallExpression(self: *Parser, function: ast.Expression) !?ast.Expression {
        const call_cur_token = self.cur_token;
        if (try self.parseExpressionList(.RParen)) |args| {
            return (try ast.CallExpression.init(
                self.allocator,
                call_cur_token,
                function,
                args,
            )).expression();
        }
        return null;
    }

    fn parseIndexExpression(self: *Parser, left: ast.Expression) !?ast.Expression {
        const index_cur_token = self.cur_token;

        self.nextToken();

        const index = try self.parseExpression(.Lowest);

        if (!(try self.expectPeek(.RBracket))) {
            return null;
        }

        return (try ast.IndexExpression.init(
            self.allocator,
            index_cur_token,
            left,
            index.?,
        )).expression();
    }

    fn parseStatement(self: *Parser) !?ast.Statement {
        switch (self.cur_token.type) {
            .Let => {
                if (try self.parseLetStatement()) |ls| {
                    return ls.statement();
                }
                return null;
            },
            .Return => {
                return (try self.parseReturnStatement()).statement();
            },
            else => {
                return (try self.parseExpressionStatement()).statement();
            },
        }
    }

    pub fn parseProgram(self: *Parser) !*ast.Program {
        const program = try ast.Program.init(self.allocator);
        while (self.cur_token.type != .Eof) {
            const stmt = try self.parseStatement();
            if (stmt) |s| {
                try program.statements.append(s);
            }
            self.nextToken();
        }

        return program;
    }

    fn peekError(self: *Parser, token_type: tok.TokenType) !void {
        const msg = try std.fmt.allocPrint(
            self.allocator,
            "expected next token to be {any}, got {any} instead",
            .{
                token_type,
                self.peek_token.type,
            },
        );
        try self.errors.append(msg);
    }

    fn noPrefixParseFnError(self: *Parser, token_type: tok.TokenType) !void {
        const msg = try std.fmt.allocPrint(
            self.allocator,
            "no prefix parse function for {any} found",
            .{token_type},
        );
        try self.errors.append(msg);
    }

    fn registerPrefix(self: *Parser, token_type: tok.TokenType, prefix_fn: PrefixParseFn) !void {
        try self.prefix_parse_fns.put(token_type, prefix_fn);
    }

    fn registerInfix(self: *Parser, token_type: tok.TokenType, infix_fn: InfixParseFn) !void {
        try self.infix_parse_fns.put(token_type, infix_fn);
    }
};

test "test let statements" {
    const let_tests = [_]struct {
        input: []const u8,
        expected_identifier: []const u8,
        expected_value: TestLiteralValue,
    }{
        .{ .input = "let x = 5;", .expected_identifier = "x", .expected_value = .{ .integer = 5 } },
        .{ .input = "let y = true;", .expected_identifier = "y", .expected_value = .{ .boolean = true } },
        .{ .input = "let foobar = y;", .expected_identifier = "foobar", .expected_value = .{ .string = "y" } },
    };

    for (let_tests) |let_test| {
        const allocator = std.testing.allocator;

        const l = try lexer.Lexer.init(allocator, let_test.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        const program = try p.parseProgram();
        defer program.deinit();
        checkParserErrors(p);

        try testing.expect(program.statements.items.len == 1);

        const let_stmt: *ast.LetStatement = @ptrCast(@alignCast(program.statements.items[0].ptr));

        try testing.expect(testLetStatement(let_stmt.statement(), let_test.expected_identifier));
        try testing.expect(try testLiteralExpression(allocator, let_stmt.value, let_test.expected_value));
    }
}

fn checkParserErrors(parser: *Parser) void {
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

fn testLetStatement(s: ast.Statement, name: []const u8) bool {
    if (!std.mem.eql(u8, s.tokenLiteral(), "let")) {
        return false;
    }

    const let_stmt: *ast.LetStatement = @ptrCast(@alignCast(s.ptr));

    if (!std.mem.eql(u8, let_stmt.name.value, name)) {
        return false;
    }
    if (!std.mem.eql(u8, let_stmt.name.tokenLiteral(), name)) {
        return false;
    }
    return true;
}

test "test return statement" {
    const return_tests = [_]struct {
        input: []const u8,
        expected_value: TestLiteralValue,
    }{
        .{ .input = "return 5;", .expected_value = .{ .integer = 5 } },
        .{ .input = "return 10;", .expected_value = .{ .integer = 10 } },
        .{ .input = "return 993322;", .expected_value = .{ .integer = 993322 } },
    };

    for (return_tests) |return_test| {
        const allocator = std.testing.allocator;

        const l = try lexer.Lexer.init(allocator, return_test.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        const program = try p.parseProgram();
        defer program.deinit();
        checkParserErrors(p);

        try testing.expect(program.statements.items.len == 1);

        const stmt = program.statements.items[0];
        const ret_stmt: *ast.ReturnStatement = @ptrCast(@alignCast(stmt.ptr));

        try testing.expect(std.mem.eql(u8, ret_stmt.tokenLiteral(), "return"));
        try testing.expect(try testLiteralExpression(allocator, ret_stmt.return_value, return_test.expected_value));
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
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();
    checkParserErrors(p);

    try testing.expect(program.statements.items.len == 1);

    const stmt = program.statements.items[0];
    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.ptr));

    const ident: *ast.Identifier = @ptrCast(@alignCast(exp_stmt.expression.ptr));
    try testing.expect(std.mem.eql(u8, ident.tokenLiteral(), "foobar"));
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
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();
    checkParserErrors(p);

    try testing.expect(program.statements.items.len == 1);

    const stmt = program.statements.items[0];
    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.ptr));

    const lit: *ast.IntegerLiteral = @ptrCast(@alignCast(exp_stmt.expression.ptr));
    try testing.expect(std.mem.eql(u8, lit.tokenLiteral(), "5"));
    try testing.expect(lit.value == 5);
}

fn testIntegerLiteral(allocator: std.mem.Allocator, il: ast.Expression, value: i64) !bool {
    const integ: *ast.IntegerLiteral = @ptrCast(@alignCast(il.ptr));

    if (integ.value != value) {
        return false;
    }

    const integ_str = try std.fmt.allocPrint(allocator, "{d}", .{value});
    defer allocator.free(integ_str);

    if (!std.mem.eql(u8, integ.tokenLiteral(), integ_str)) {
        return false;
    }

    return true;
}

test "test string literal expression" {
    const input =
        \\"hello world";
    ;

    const allocator = std.testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();
    checkParserErrors(p);

    try testing.expect(program.statements.items.len == 1);
    const stmt = program.statements.items[0];
    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.ptr));
    const str_lit: *ast.StringLiteral = @ptrCast(@alignCast(exp_stmt.expression.ptr));
    try testing.expect(std.mem.eql(u8, str_lit.tokenLiteral(), "hello world"));
}

const TestLiteralValue = union(enum) {
    integer: i64,
    boolean: bool,
    string: []const u8,
};

test "test parsing prefix expressions" {
    const prefix_tests = [_]struct {
        input: []const u8,
        operator: []const u8,
        value: TestLiteralValue,
    }{
        .{ .input = "!5", .operator = "!", .value = TestLiteralValue{ .integer = 5 } },
        .{ .input = "-15", .operator = "-", .value = TestLiteralValue{ .integer = 15 } },
        .{ .input = "!true", .operator = "!", .value = TestLiteralValue{ .boolean = true } },
        .{ .input = "!false", .operator = "!", .value = TestLiteralValue{ .boolean = false } },
    };

    for (prefix_tests) |prefix_test| {
        const allocator = std.testing.allocator;

        const l = try lexer.Lexer.init(allocator, prefix_test.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        const program = try p.parseProgram();
        defer program.deinit();
        checkParserErrors(p);

        try testing.expect(program.statements.items.len == 1);

        const stmt = program.statements.items[0];
        const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.ptr));

        const prefix_expr: *ast.PrefixExpression = @ptrCast(@alignCast(exp_stmt.expression.ptr));
        try testing.expect(std.mem.eql(u8, prefix_expr.operator, prefix_test.operator));
        try testing.expect(try testPrefixExpression(allocator, prefix_expr.expression(), prefix_expr.operator, prefix_test.value));
    }
}

test "test parsing infix expressions" {
    const infix_tests = [_]struct {
        input: []const u8,
        left_value: TestLiteralValue,
        operator: []const u8,
        right_value: TestLiteralValue,
    }{
        .{ .input = "5 + 5;", .left_value = TestLiteralValue{ .integer = 5 }, .operator = "+", .right_value = TestLiteralValue{ .integer = 5 } },
        .{ .input = "5 - 5;", .left_value = TestLiteralValue{ .integer = 5 }, .operator = "-", .right_value = TestLiteralValue{ .integer = 5 } },
        .{ .input = "5 * 5;", .left_value = TestLiteralValue{ .integer = 5 }, .operator = "*", .right_value = TestLiteralValue{ .integer = 5 } },
        .{ .input = "5 / 5;", .left_value = TestLiteralValue{ .integer = 5 }, .operator = "/", .right_value = TestLiteralValue{ .integer = 5 } },
        .{ .input = "5 > 5;", .left_value = TestLiteralValue{ .integer = 5 }, .operator = ">", .right_value = TestLiteralValue{ .integer = 5 } },
        .{ .input = "5 < 5;", .left_value = TestLiteralValue{ .integer = 5 }, .operator = "<", .right_value = TestLiteralValue{ .integer = 5 } },
        .{ .input = "5 == 5;", .left_value = TestLiteralValue{ .integer = 5 }, .operator = "==", .right_value = TestLiteralValue{ .integer = 5 } },
        .{ .input = "5 != 5;", .left_value = TestLiteralValue{ .integer = 5 }, .operator = "!=", .right_value = TestLiteralValue{ .integer = 5 } },
        .{ .input = "true == true", .left_value = TestLiteralValue{ .boolean = true }, .operator = "==", .right_value = TestLiteralValue{ .boolean = true } },
        .{ .input = "true != false", .left_value = TestLiteralValue{ .boolean = true }, .operator = "!=", .right_value = TestLiteralValue{ .boolean = false } },
        .{ .input = "false == false", .left_value = TestLiteralValue{ .boolean = false }, .operator = "==", .right_value = TestLiteralValue{ .boolean = false } },
    };

    for (infix_tests) |infix_test| {
        const allocator = std.testing.allocator;

        const l = try lexer.Lexer.init(allocator, infix_test.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        const program = try p.parseProgram();
        defer program.deinit();
        checkParserErrors(p);

        try testing.expect(program.statements.items.len == 1);

        const stmt = program.statements.items[0];
        const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.ptr));

        const infix_expr: *ast.InfixExpression = @ptrCast(@alignCast(exp_stmt.expression.ptr));
        try testing.expect(try testInfixExpression(allocator, infix_expr.expression(), infix_test.left_value, infix_test.operator, infix_test.right_value));
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
        .{ .input = "true", .expected = "true" },
        .{ .input = "false", .expected = "false" },
        .{ .input = "3 > 5 == false", .expected = "((3 > 5) == false)" },
        .{ .input = "3 < 5 == true", .expected = "((3 < 5) == true)" },
        .{ .input = "1 + (2 + 3) + 4", .expected = "((1 + (2 + 3)) + 4)" },
        .{ .input = "(5 + 5) * 2", .expected = "((5 + 5) * 2)" },
        .{ .input = "2 / (5 + 5)", .expected = "(2 / (5 + 5))" },
        .{ .input = "-(5 + 5)", .expected = "(-(5 + 5))" },
        .{ .input = "!(true == true)", .expected = "(!(true == true))" },
        .{ .input = "a + add(b * c) + d", .expected = "((a + add((b * c))) + d)" },
        .{ .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
        .{ .input = "add(a + b + c * d / f + g)", .expected = "add((((a + b) + ((c * d) / f)) + g))" },
        .{ .input = "a * [1, 2, 3, 4][b * c] * d", .expected = "((a * ([1, 2, 3, 4][(b * c)])) * d)" },
        .{ .input = "add(a * b[2], b[1], 2 * [1, 2][1])", .expected = "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))" },
    };

    for (precedence_tests) |precedence_test| {
        const allocator = std.testing.allocator;

        const l = try lexer.Lexer.init(allocator, precedence_test.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        const program = try p.parseProgram();
        defer program.deinit();
        checkParserErrors(p);

        const program_str = try program.string();
        try testing.expect(std.mem.eql(u8, program_str, precedence_test.expected));
    }
}

fn testIdentifier(exp: ast.Expression, value: []const u8) bool {
    const ident: *ast.Identifier = @ptrCast(@alignCast(exp.ptr));

    if (!std.mem.eql(u8, ident.value, value)) return false;
    if (!std.mem.eql(u8, ident.tokenLiteral(), value)) return false;

    return true;
}

fn testBooleanLiteral(exp: ast.Expression, value: bool) bool {
    const boolean_exp: *ast.Boolean = @ptrCast(@alignCast(exp.ptr));

    if (boolean_exp.value != value) return false;

    if (!std.mem.eql(u8, boolean_exp.tokenLiteral(), if (value) "true" else "false")) return false;

    return true;
}

fn testLiteralExpression(allocator: std.mem.Allocator, exp: ast.Expression, expected: TestLiteralValue) !bool {
    switch (expected) {
        .integer => |v| return testIntegerLiteral(allocator, exp, v),
        .string => |v| return testIdentifier(exp, v),
        .boolean => |v| return testBooleanLiteral(exp, v),
    }

    return false;
}

fn testPrefixExpression(allocator: std.mem.Allocator, exp: ast.Expression, operator: []const u8, right: TestLiteralValue) !bool {
    const prefix_exp: *ast.PrefixExpression = @ptrCast(@alignCast(exp.ptr));

    if (!std.mem.eql(u8, prefix_exp.operator, operator)) {
        return false;
    }

    if (!(try testLiteralExpression(allocator, prefix_exp.right, right))) {
        return false;
    }

    return true;
}

fn testInfixExpression(allocator: std.mem.Allocator, exp: ast.Expression, left: TestLiteralValue, operator: []const u8, right: TestLiteralValue) !bool {
    const infix_exp: *ast.InfixExpression = @ptrCast(@alignCast(exp.ptr));

    if (!(try testLiteralExpression(allocator, infix_exp.left, left))) {
        return false;
    }

    if (!std.mem.eql(u8, infix_exp.operator, operator)) {
        return false;
    }

    if (!(try testLiteralExpression(allocator, infix_exp.right, right))) {
        return false;
    }

    return true;
}

test "test boolean expression" {
    const boolean_tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "true;", .expected = true },
        .{ .input = "false;", .expected = false },
    };

    for (boolean_tests) |boolean_test| {
        const allocator = std.testing.allocator;

        const l = try lexer.Lexer.init(allocator, boolean_test.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        const program = try p.parseProgram();
        defer program.deinit();
        checkParserErrors(p);

        const stmt = program.statements.items[0];
        const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(stmt.ptr));

        const boolean_exp: *ast.Boolean = @ptrCast(@alignCast(exp_stmt.expression.ptr));
        try testing.expect(boolean_exp.value == boolean_test.expected);
    }
}

test "test if expression" {
    const input = "if (x < y) { x }";

    const allocator = testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();
    checkParserErrors(p);

    try testing.expect(program.statements.items.len == 1);

    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(program.statements.items[0].ptr));

    const if_exp: *ast.IfExpression = @ptrCast(@alignCast(exp_stmt.expression.ptr));

    try testing.expect(try testInfixExpression(allocator, if_exp.condition, .{ .string = "x" }, "<", .{ .string = "y" }));
    try testing.expect(if_exp.consequence.statements.items.len == 1);

    const consequence_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(if_exp.consequence.statements.items[0].ptr));
    try testing.expect(testIdentifier(consequence_stmt.expression, "x"));

    try testing.expect(if_exp.alternative == null);
}

test "test if else expression" {
    const input = "if (x < y) { x } else { y }";

    const allocator = testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();
    checkParserErrors(p);

    try testing.expect(program.statements.items.len == 1);

    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(program.statements.items[0].ptr));

    const if_exp: *ast.IfExpression = @ptrCast(@alignCast(exp_stmt.expression.ptr));

    try testing.expect(try testInfixExpression(allocator, if_exp.condition, .{ .string = "x" }, "<", .{ .string = "y" }));
    try testing.expect(if_exp.consequence.statements.items.len == 1);

    const consequence_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(if_exp.consequence.statements.items[0].ptr));
    try testing.expect(testIdentifier(consequence_stmt.expression, "x"));

    try testing.expect(if_exp.alternative.?.statements.items.len == 1);

    const alternative_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(if_exp.alternative.?.statements.items[0].ptr));
    try testing.expect(testIdentifier(alternative_stmt.expression, "y"));
}

test "test function literal parsing" {
    const input = "fn(x, y) { x + y; }";

    const allocator = testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();
    checkParserErrors(p);

    try testing.expect(program.statements.items.len == 1);

    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(program.statements.items[0].ptr));

    const func_literal: *ast.FunctionLiteral = @ptrCast(@alignCast(exp_stmt.expression.ptr));
    try testing.expect(func_literal.parameters.items.len == 2);

    try testing.expect(try testLiteralExpression(allocator, func_literal.parameters.items[0].expression(), .{ .string = "x" }));
    try testing.expect(try testLiteralExpression(allocator, func_literal.parameters.items[1].expression(), .{ .string = "y" }));

    try testing.expect(func_literal.body.statements.items.len == 1);

    const body_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(func_literal.body.statements.items[0].ptr));
    try testing.expect(try testInfixExpression(allocator, body_stmt.expression, .{ .string = "x" }, "+", .{ .string = "y" }));
}

test "test function parameter parsing" {
    const param_tests = [_]struct {
        input: []const u8,
        expected_params: []const u8,
    }{
        .{ .input = "fn() {};", .expected_params = "" },
        .{ .input = "fn(x) {};", .expected_params = "x" },
        .{ .input = "fn(x, y, z) {};", .expected_params = "xyz" },
    };

    for (param_tests) |param_test| {
        const allocator = testing.allocator;

        const l = try lexer.Lexer.init(allocator, param_test.input);
        defer allocator.destroy(l);

        const p = try Parser.init(allocator, l);
        defer p.deinit();

        const program = try p.parseProgram();
        defer program.deinit();
        checkParserErrors(p);

        const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(program.statements.items[0].ptr));
        const func_literal: *ast.FunctionLiteral = @ptrCast(@alignCast(exp_stmt.expression.ptr));

        try testing.expect(func_literal.parameters.items.len == param_test.expected_params.len);
        for (func_literal.parameters.items, 0..) |param, index| {
            const expected = param_test.expected_params[index];
            try testing.expect(try testLiteralExpression(allocator, param.expression(), .{ .string = (&[_]u8{expected})[0..] }));
        }
    }
}

test "test call expression parsing" {
    const input = "add(1, 2 * 3, 4 + 5)";

    const allocator = testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();
    checkParserErrors(p);

    try testing.expect(program.statements.items.len == 1);

    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(program.statements.items[0].ptr));

    const call_exp: *ast.CallExpression = @ptrCast(@alignCast(exp_stmt.expression.ptr));
    try testing.expect(testIdentifier(call_exp.function, "add"));

    try testing.expect(call_exp.arguments.items.len == 3);

    try testing.expect(try testLiteralExpression(allocator, call_exp.arguments.items[0], .{ .integer = 1 }));
    try testing.expect(try testInfixExpression(allocator, call_exp.arguments.items[1], .{ .integer = 2 }, "*", .{ .integer = 3 }));
    try testing.expect(try testInfixExpression(allocator, call_exp.arguments.items[2], .{ .integer = 4 }, "+", .{ .integer = 5 }));
}

test "test array literals parsing" {
    const input = "[1, 2 * 2, 3 + 3]";

    const allocator = testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();
    checkParserErrors(p);

    try testing.expect(program.statements.items.len == 1);

    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(program.statements.items[0].ptr));

    const array_literal: *ast.ArrayLiteral = @ptrCast(@alignCast(exp_stmt.expression.ptr));
    try testing.expect(array_literal.elements.items.len == 3);

    try testing.expect(try testLiteralExpression(allocator, array_literal.elements.items[0], .{ .integer = 1 }));
    try testing.expect(try testInfixExpression(allocator, array_literal.elements.items[1], .{ .integer = 2 }, "*", .{ .integer = 2 }));
    try testing.expect(try testInfixExpression(allocator, array_literal.elements.items[2], .{ .integer = 3 }, "+", .{ .integer = 3 }));
}

test "test parsing index expression" {
    const input = "myArray[1 + 1]";

    const allocator = testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();
    checkParserErrors(p);

    try testing.expect(program.statements.items.len == 1);

    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(program.statements.items[0].ptr));

    const index_exp: *ast.IndexExpression = @ptrCast(@alignCast(exp_stmt.expression.ptr));
    try testing.expect(testIdentifier(index_exp.left, "myArray"));
    try testing.expect(try testInfixExpression(allocator, index_exp.index, .{ .integer = 1 }, "+", .{ .integer = 1 }));
}

test "test parsing hash literals string keys" {
    const input =
        \\{"one": 1, "two": 2, "three": 3};
    ;

    const allocator = testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();
    checkParserErrors(p);

    try testing.expect(program.statements.items.len == 1);

    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(program.statements.items[0].ptr));

    const hash_literal: *ast.HashLiteral = @ptrCast(@alignCast(exp_stmt.expression.ptr));

    var expected = std.StringHashMap(i64).init(allocator);
    defer expected.deinit();
    try expected.put("one", 1);
    try expected.put("two", 2);
    try expected.put("three", 3);

    try testing.expect(hash_literal.pairs.count() == expected.count());

    var it = hash_literal.pairs.iterator();
    while (it.next()) |pair| {
        const key_ptr = pair.key_ptr.*.ptr;
        const key_str: *ast.StringLiteral = @ptrCast(@alignCast(key_ptr));
        const value_ptr = pair.value_ptr.*.ptr;
        const value: *ast.IntegerLiteral = @ptrCast(@alignCast(value_ptr));

        const expected_value = expected.get(key_str.value);
        if (expected_value) |v| {
            try testing.expect(value.value == v);
        } else {
            try testing.expect(false);
        }
    }
}

test "test parsing empty hash literals" {
    const input = "{}";

    const allocator = testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();
    checkParserErrors(p);

    try testing.expect(program.statements.items.len == 1);

    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(program.statements.items[0].ptr));

    const hash_literal: *ast.HashLiteral = @ptrCast(@alignCast(exp_stmt.expression.ptr));
    try testing.expect(hash_literal.pairs.count() == 0);
}
test "test parsing hash literals with expressions" {
    const input =
        \\{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5};
    ;

    const allocator = testing.allocator;

    const l = try lexer.Lexer.init(allocator, input);
    defer allocator.destroy(l);

    const p = try Parser.init(allocator, l);
    defer p.deinit();

    const program = try p.parseProgram();
    defer program.deinit();
    checkParserErrors(p);

    try testing.expect(program.statements.items.len == 1);

    const exp_stmt: *ast.ExpressionStatement = @ptrCast(@alignCast(program.statements.items[0].ptr));

    const hash_literal: *ast.HashLiteral = @ptrCast(@alignCast(exp_stmt.expression.ptr));

    var expected = std.StringHashMap(struct { left: TestLiteralValue, operator: []const u8, right: TestLiteralValue }).init(allocator);
    defer expected.deinit();
    try expected.put("one", .{ .left = .{ .integer = 0 }, .operator = "+", .right = .{ .integer = 1 } });
    try expected.put("two", .{ .left = .{ .integer = 10 }, .operator = "-", .right = .{ .integer = 8 } });
    try expected.put("three", .{ .left = .{ .integer = 15 }, .operator = "/", .right = .{ .integer = 5 } });

    try testing.expect(hash_literal.pairs.count() == expected.count());

    var it = hash_literal.pairs.iterator();
    while (it.next()) |pair| {
        const key_ptr = pair.key_ptr.*.ptr;
        const key_str: *ast.StringLiteral = @ptrCast(@alignCast(key_ptr));
        const value_ptr = pair.value_ptr.*;

        const expected_value = expected.get(key_str.value);
        if (expected_value) |v| {
            try testing.expect(try testInfixExpression(allocator, value_ptr, v.left, v.operator, v.right));
        } else {
            try testing.expect(false);
        }
    }
}
