const std = @import("std");
const testing = std.testing;
const tok = @import("token");
const TokenType = tok.TokenType;
const Token = tok.Token;

const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: u8,

    pub fn init(allocator: std.mem.Allocator, input: []const u8) !*Lexer {
        const lexer = try allocator.create(Lexer);
        lexer.* = .{
            .input = input,
            .position = 0,
            .read_position = 0,
            .ch = 0,
        };
        lexer.read_char(); // Initialize the first character
        return lexer;
    }

    fn read_char(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0; // EOF
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(self: *Lexer) Token {
        var token: Token = .{ .type = .Illegal, .literal = "" };

        switch (self.ch) {
            '=' => {
                token = new_token(.Assign, "=");
            },
            ';' => {
                token = new_token(.Semicolon, ";");
            },
            '(' => {
                token = new_token(.LParen, "(");
            },
            ')' => {
                token = new_token(.RParen, ")");
            },
            ',' => {
                token = new_token(.Comma, ",");
            },
            '+' => {
                token = new_token(.Plus, "+");
            },
            '{' => {
                token = new_token(.LBrace, "{");
            },
            '}' => {
                token = new_token(.RBrace, "}");
            },
            0 => {
                token = new_token(.Eof, "");
            },
            else => {},
        }

        self.read_char();
        return token;
    }
};

fn new_token(token_type: TokenType, literal: []const u8) Token {
    return .{ .type = token_type, .literal = literal };
}

test "test next token" {
    const input = "=+(){},;";

    const expected = [_]struct {
        expected_type: TokenType,
        expected_literal: []const u8,
    }{
        .{ .expected_type = .Assign, .expected_literal = "=" },
        .{ .expected_type = .Plus, .expected_literal = "+" },
        .{ .expected_type = .LParen, .expected_literal = "(" },
        .{ .expected_type = .RParen, .expected_literal = ")" },
        .{ .expected_type = .LBrace, .expected_literal = "{" },
        .{ .expected_type = .RBrace, .expected_literal = "}" },
        .{ .expected_type = .Comma, .expected_literal = "," },
        .{ .expected_type = .Semicolon, .expected_literal = ";" },
        .{ .expected_type = .Eof, .expected_literal = "" },
    };

    const l = try Lexer.init(std.testing.allocator, input);
    defer std.testing.allocator.destroy(l);

    for (expected) |e| {
        const token = l.next_token();

        try testing.expectEqual(e.expected_type, token.type);
        try testing.expectEqual(e.expected_literal, token.literal);
    }
}
