const std = @import("std");
const testing = std.testing;
const lexer = @import("lexer");
const Token = @import("token").Token;
const ast = @import("ast");

const Parser = struct {
    l: *lexer.Lexer,
    curToken: Token,
    peekToken: Token,

    fn init(allocator: std.mem.Allocator, l: *lexer.Lexer) !*Parser {
        var p = try allocator.create(Parser);
        p.* = .{
            .l = l,
            .curToken = .{ .type = .Illegal, .literal = "" },
            .peekToken = .{ .type = .Illegal, .literal = "" },
        };
        p.next_token();
        p.next_token();
        return p;
    }

    fn next_token(self: *Parser) void {
        self.curToken = self.peekToken;
        self.peekToken = self.l.next_token();
    }

    fn parse_program(_: *Parser, _: std.mem.Allocator) *ast.Program {
        return null;
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

    const program = p.parse_program(allocator);
    defer allocator.destroy(program);

    try testing.expect(program != null);
    try testing.expect(program.statements.len == 3);

    const expected = [_]struct {
        identifier: []const u8,
    }{
        .{ .identifier = "x" },
        .{ .identifier = "y" },
        .{ .identifier = "foobar" },
    };

    for (expected, 0..) |exp, i| {
        const stmt = program.statements[i];
        try testing.expect(test_let_statement(stmt, exp.identifier));
    }
}

fn test_let_statement(s: ast.Statement, name: []const u8) bool {
    if (!std.mem.eql(u8, s.token_literal(), name)) {
        return false;
    }

    const let_stmt: *ast.LetStatement = @ptrCast(s.ptr);

    if (!std.mem.eql(u8, let_stmt.name.value, name)) {
        return false;
    }
    if (!std.mem.eql(u8, let_stmt.name.token_literal(), name)) {
        return false;
    }
    return true;
}
