use crate::lexer::span::Span;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,

    Identifier,

    Number,
    String,
    Char,

    Assign, // :=
    EqEq,   // ==
    NotEq,  // !=
    Gt,     // >
    Lt,     // <
    LtEq,   // <=
    GtEq,   // >=
    Not,    // !
    And,    // &&
    Or,     // ||

    Inc,     // ++
    Dec,     // --
    Asterix, // *
    Slash,   // /
    Plus,    // +
    Minus,   // -
    Dot,     // .
    Mod,     // %
    TypeOf,  // typeof

    Semicolon,  // ;
    Comma,      // ,
    LParen,     // (
    RParen,     // )
    LSqBracket, // [
    RSqBracket, // ]
    LBrace,     // {
    RBrace,     // }

    Let,      // let
    Function, // fn
    True,     // true
    False,    // false
    If,       // if
    Else,     // else
    Return,   // return
}

#[derive(Debug)]
pub struct Token<'a> {
    pub t_type: TokenType,
    pub lexeme: &'a str,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new(
        t_type: TokenType,
        lexeme: &'a str,
        start_byte_pos: usize,
        end_byte_pos: usize,
        line_num: usize,
        col_num: usize,
    ) -> Self {
        Token {
            t_type,
            lexeme,
            span: Span {
                start_byte_pos,
                end_byte_pos,
                line_num,
                col_num,
            },
        }
    }
}
