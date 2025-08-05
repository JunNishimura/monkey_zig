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

const Token = struct {
    type: TokenType,
    literal: []u8,
};
