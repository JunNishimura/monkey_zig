const std = @import("std");
const testing = std.testing;
const lexer = @import("lexer");
const tok = @import("token");
const ast = @import("ast");

const Parser = struct {
    l: *lexer.Lexer,
    cur_token: tok.Token,
    peek_token: tok.Token,

    fn init(allocator: std.mem.Allocator, l: *lexer.Lexer) !*Parser {
        var p = try allocator.create(Parser);
        p.* = .{
            .l = l,
            .cur_token = .{ .type = .Illegal, .literal = "" },
            .peek_token = .{ .type = .Illegal, .literal = "" },
        };
        p.next_token();
        p.next_token();
        return p;
    }

    fn next_token(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.l.next_token();
    }

    fn peek_token_is(self: *Parser, token_type: tok.TokenType) bool {
        return self.peek_token.type == token_type;
    }

    fn expect_peek(self: *Parser, token_type: tok.TokenType) bool {
        if (self.peek_token_is(token_type)) {
            self.next_token();
            return true;
        }
        return false;
    }

    fn cur_token_is(self: *Parser, token_type: tok.TokenType) bool {
        return self.cur_token.type == token_type;
    }

    fn parse_let_statement(self: *Parser, allocator: std.mem.Allocator) !?*ast.LetStatement {
        const let_stmt = try ast.LetStatement.init(allocator, self.cur_token);

        if (!self.expect_peek(.Ident)) {
            return null;
        }

        let_stmt.name = try ast.Identifier.init(allocator, self.cur_token, self.cur_token.literal);

        if (!self.expect_peek(.Assign)) {
            return null;
        }

        while (!self.cur_token_is(.Semicolon)) {
            self.next_token();
        }

        return let_stmt;
    }

    fn parse_statement(self: *Parser, allocator: std.mem.Allocator) !?ast.Statement {
        switch (self.cur_token.type) {
            .Let => return {
                const let_stmt = try self.parse_let_statement(allocator);
                if (let_stmt) |ls| {
                    return ls.statement();
                }
                return null;
            },
            else => return null,
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
    defer allocator.destroy(p);

    const program = try p.parse_program(allocator);
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
