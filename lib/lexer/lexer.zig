const std = @import("std");
const testing = std.testing;
const tok = @import("token");
const TokenType = tok.TokenType;
const Token = tok.Token;

pub const Lexer = struct {
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
        lexer.readChar(); // Initialize the first character
        return lexer;
    }

    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0; // EOF
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        }
        return self.input[self.read_position];
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const start_position = self.position;
        while (isLetter(self.ch)) {
            self.readChar();
        }
        return self.input[start_position..self.position];
    }

    fn readNumber(self: *Lexer) []const u8 {
        const start_position = self.position;
        while (isDigit(self.ch)) {
            self.readChar();
        }
        return self.input[start_position..self.position];
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
            self.readChar();
        }
    }

    fn readString(self: *Lexer) []const u8 {
        const position = self.position + 1; // Skip the opening quote
        while (true) {
            self.readChar();
            if (self.ch == '"' or self.ch == 0) {
                break;
            }
        }
        return self.input[position..self.position];
    }

    pub fn nextToken(self: *Lexer) Token {
        var token: Token = .{ .type = .Illegal, .literal = "" };

        self.skipWhitespace();

        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    self.readChar();
                    token = newToken(.Eq, "==");
                } else {
                    token = newToken(.Assign, "=");
                }
            },
            ';' => {
                token = newToken(.Semicolon, ";");
            },
            ':' => {
                token = newToken(.Colon, ":");
            },
            '(' => {
                token = newToken(.LParen, "(");
            },
            ')' => {
                token = newToken(.RParen, ")");
            },
            ',' => {
                token = newToken(.Comma, ",");
            },
            '+' => {
                token = newToken(.Plus, "+");
            },
            '-' => {
                token = newToken(.Minus, "-");
            },
            '!' => {
                if (self.peekChar() == '=') {
                    self.readChar();
                    token = newToken(.NotEq, "!=");
                } else {
                    token = newToken(.Bang, "!");
                }
            },
            '/' => {
                token = newToken(.Slash, "/");
            },
            '*' => {
                token = newToken(.Asterisk, "*");
            },
            '<' => {
                token = newToken(.LessThan, "<");
            },
            '>' => {
                token = newToken(.GreaterThan, ">");
            },
            '{' => {
                token = newToken(.LBrace, "{");
            },
            '}' => {
                token = newToken(.RBrace, "}");
            },
            '[' => {
                token = newToken(.LBracket, "[");
            },
            ']' => {
                token = newToken(.RBracket, "]");
            },
            '"' => {
                token.type = .String;
                token.literal = self.readString();
            },
            0 => {
                token = newToken(.Eof, "");
            },
            else => {
                if (isLetter(self.ch)) {
                    token.literal = self.readIdentifier();
                    token.type = tok.lookupIdentifier(token.literal);
                    return token;
                } else if (isDigit(self.ch)) {
                    token.literal = self.readNumber();
                    token.type = .Int;
                    return token;
                } else {
                    token = newToken(.Illegal, "");
                }
            },
        }

        self.readChar();
        return token;
    }
};

fn newToken(token_type: TokenType, literal: []const u8) Token {
    return .{ .type = token_type, .literal = literal };
}

fn isLetter(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or (ch == '_');
}

fn isDigit(ch: u8) bool {
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
        \\
        \\if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
        \\"foobar"
        \\"foo bar"
        \\[1, 2];
        \\{"foo": "bar"}
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
        .{ .expected_type = .If, .expected_literal = "if" },
        .{ .expected_type = .LParen, .expected_literal = "(" },
        .{ .expected_type = .Int, .expected_literal = "5" },
        .{ .expected_type = .LessThan, .expected_literal = "<" },
        .{ .expected_type = .Int, .expected_literal = "10" },
        .{ .expected_type = .RParen, .expected_literal = ")" },
        .{ .expected_type = .LBrace, .expected_literal = "{" },
        .{ .expected_type = .Return, .expected_literal = "return" },
        .{ .expected_type = .True, .expected_literal = "true" },
        .{ .expected_type = .Semicolon, .expected_literal = ";" },
        .{ .expected_type = .RBrace, .expected_literal = "}" },
        .{ .expected_type = .Else, .expected_literal = "else" },
        .{ .expected_type = .LBrace, .expected_literal = "{" },
        .{ .expected_type = .Return, .expected_literal = "return" },
        .{ .expected_type = .False, .expected_literal = "false" },
        .{ .expected_type = .Semicolon, .expected_literal = ";" },
        .{ .expected_type = .RBrace, .expected_literal = "}" },
        .{ .expected_type = .Int, .expected_literal = "10" },
        .{ .expected_type = .Eq, .expected_literal = "==" },
        .{ .expected_type = .Int, .expected_literal = "10" },
        .{ .expected_type = .Semicolon, .expected_literal = ";" },
        .{ .expected_type = .Int, .expected_literal = "10" },
        .{ .expected_type = .NotEq, .expected_literal = "!=" },
        .{ .expected_type = .Int, .expected_literal = "9" },
        .{ .expected_type = .Semicolon, .expected_literal = ";" },
        .{ .expected_type = .String, .expected_literal = "foobar" },
        .{ .expected_type = .String, .expected_literal = "foo bar" },
        .{ .expected_type = .LBracket, .expected_literal = "[" },
        .{ .expected_type = .Int, .expected_literal = "1" },
        .{ .expected_type = .Comma, .expected_literal = "," },
        .{ .expected_type = .Int, .expected_literal = "2" },
        .{ .expected_type = .RBracket, .expected_literal = "]" },
        .{ .expected_type = .Semicolon, .expected_literal = ";" },
        .{ .expected_type = .LBrace, .expected_literal = "{" },
        .{ .expected_type = .String, .expected_literal = "foo" },
        .{ .expected_type = .Colon, .expected_literal = ":" },
        .{ .expected_type = .String, .expected_literal = "bar" },
        .{ .expected_type = .RBrace, .expected_literal = "}" },
        .{ .expected_type = .Eof, .expected_literal = "" },
    };

    const allocator = testing.allocator;
    const l = try Lexer.init(allocator, input);
    defer allocator.destroy(l);

    for (expected) |e| {
        const token = l.nextToken();

        try testing.expectEqual(e.expected_type, token.type);
        try testing.expectEqualStrings(e.expected_literal, token.literal);
    }
}
