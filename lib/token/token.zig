pub const TokenType = enum {
    Illegal,
    Eof,

    Ident,
    Int,

    Assign,
    Plus,

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
