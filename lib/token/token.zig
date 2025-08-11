const std = @import("std");

pub const TokenType = enum {
    Illegal,
    Eof,

    Ident,
    Int,

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LessThan,
    GreaterThan,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
};

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
};

const Keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "fn", .Function },
    .{ "let", .Let },
});

pub fn lookup_identifier(ident: []const u8) TokenType {
    return Keywords.get(ident) orelse .Ident;
}
