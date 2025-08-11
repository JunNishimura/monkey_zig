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

    Eq,
    NotEq,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
};

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
};

const Keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "fn", .Function },
    .{ "let", .Let },
    .{ "true", .True },
    .{ "false", .False },
    .{ "if", .If },
    .{ "else", .Else },
    .{ "return", .Return },
});

pub fn lookup_identifier(ident: []const u8) TokenType {
    return Keywords.get(ident) orelse .Ident;
}
