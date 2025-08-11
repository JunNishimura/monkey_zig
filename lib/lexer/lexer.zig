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

    fn read_identifier(self: *Lexer) []const u8 {
        const start_position = self.position;
        while (is_letter(self.ch)) {
            self.read_char();
        }
        return self.input[start_position..self.position];
    }

    fn read_number(self: *Lexer) []const u8 {
        const start_position = self.position;
        while (is_digit(self.ch)) {
            self.read_char();
        }
        return self.input[start_position..self.position];
    }

    fn skip_whitespace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
            self.read_char();
        }
    }

    pub fn next_token(self: *Lexer) Token {
        var token: Token = .{ .type = .Illegal, .literal = "" };

        self.skip_whitespace();

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
            '-' => {
                token = new_token(.Minus, "-");
            },
            '!' => {
                token = new_token(.Bang, "!");
            },
            '/' => {
                token = new_token(.Slash, "/");
            },
            '*' => {
                token = new_token(.Asterisk, "*");
            },
            '<' => {
                token = new_token(.LessThan, "<");
            },
            '>' => {
                token = new_token(.GreaterThan, ">");
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
            else => {
                if (is_letter(self.ch)) {
                    token.literal = self.read_identifier();
                    token.type = tok.lookup_identifier(token.literal);
                    return token;
                } else if (is_digit(self.ch)) {
                    token.literal = self.read_number();
                    token.type = .Int;
                    return token;
                } else {
                    token = new_token(.Illegal, "");
                }
            },
        }

        self.read_char();
        return token;
    }
};

fn new_token(token_type: TokenType, literal: []const u8) Token {
    return .{ .type = token_type, .literal = literal };
}

fn is_letter(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or (ch == '_');
}

fn is_digit(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}

test "test next token" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\    x + y
        \\};
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
    ;

    const expected = [_]struct {
        expected_type: TokenType,
        expected_literal: []const u8,
    }{
        .{ .expected_type = .Let, .expected_literal = "let" },
        .{ .expected_type = .Ident, .expected_literal = "five" },
        .{ .expected_type = .Assign, .expected_literal = "=" },
        .{ .expected_type = .Int, .expected_literal = "5" },
        .{ .expected_type = .Semicolon, .expected_literal = ";" },
        .{ .expected_type = .Let, .expected_literal = "let" },
        .{ .expected_type = .Ident, .expected_literal = "ten" },
        .{ .expected_type = .Assign, .expected_literal = "=" },
        .{ .expected_type = .Int, .expected_literal = "10" },
        .{ .expected_type = .Semicolon, .expected_literal = ";" },
        .{ .expected_type = .Let, .expected_literal = "let" },
        .{ .expected_type = .Ident, .expected_literal = "add" },
        .{ .expected_type = .Assign, .expected_literal = "=" },
        .{ .expected_type = .Function, .expected_literal = "fn" },
        .{ .expected_type = .LParen, .expected_literal = "(" },
        .{ .expected_type = .Ident, .expected_literal = "x" },
        .{ .expected_type = .Comma, .expected_literal = "," },
        .{ .expected_type = .Ident, .expected_literal = "y" },
        .{ .expected_type = .RParen, .expected_literal = ")" },
        .{ .expected_type = .LBrace, .expected_literal = "{" },
        .{ .expected_type = .Ident, .expected_literal = "x" },
        .{ .expected_type = .Plus, .expected_literal = "+" },
        .{ .expected_type = .Ident, .expected_literal = "y" },
        .{ .expected_type = .RBrace, .expected_literal = "}" },
        .{ .expected_type = .Semicolon, .expected_literal = ";" },
        .{ .expected_type = .Let, .expected_literal = "let" },
        .{ .expected_type = .Ident, .expected_literal = "result" },
        .{ .expected_type = .Assign, .expected_literal = "=" },
        .{ .expected_type = .Ident, .expected_literal = "add" },
        .{ .expected_type = .LParen, .expected_literal = "(" },
        .{ .expected_type = .Ident, .expected_literal = "five" },
        .{ .expected_type = .Comma, .expected_literal = "," },
        .{ .expected_type = .Ident, .expected_literal = "ten" },
        .{ .expected_type = .RParen, .expected_literal = ")" },
        .{ .expected_type = .Semicolon, .expected_literal = ";" },
        .{ .expected_type = .Bang, .expected_literal = "!" },
        .{ .expected_type = .Minus, .expected_literal = "-" },
        .{ .expected_type = .Slash, .expected_literal = "/" },
        .{ .expected_type = .Asterisk, .expected_literal = "*" },
        .{ .expected_type = .Int, .expected_literal = "5" },
        .{ .expected_type = .Semicolon, .expected_literal = ";" },
        .{ .expected_type = .Int, .expected_literal = "5" },
        .{ .expected_type = .LessThan, .expected_literal = "<" },
        .{ .expected_type = .Int, .expected_literal = "10" },
        .{ .expected_type = .GreaterThan, .expected_literal = ">" },
        .{ .expected_type = .Int, .expected_literal = "5" },
        .{ .expected_type = .Semicolon, .expected_literal = ";" },
        .{ .expected_type = .Eof, .expected_literal = "" },
    };

    const l = try Lexer.init(std.testing.allocator, input);
    defer std.testing.allocator.destroy(l);

    for (expected) |e| {
        const token = l.next_token();

        try testing.expectEqual(e.expected_type, token.type);
        try testing.expectEqualStrings(e.expected_literal, token.literal);
    }
}
