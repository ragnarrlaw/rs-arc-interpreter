pub mod span;
pub mod token;

use crate::{
    error::{diagnostic::Diagnostic, lexer_error::LexerError},
    lexer::span::Span,
    lexer::token::{Token, TokenType},
};
use std::{iter::Peekable, str::CharIndices};

#[derive(Debug)]
pub struct Lexer<'a> {
    pub source: &'a str,
    pub source_itr: Peekable<CharIndices<'a>>,
    pub ch: Option<(usize, char)>, // track the byte position of the current character and the character
    pub line_num: usize,           // track the current line number (one-based)
    pub col_num: usize,            // track the current column number (one-based)
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer {
            source,
            source_itr: source.char_indices().peekable(),
            ch: None,
            line_num: 1,
            col_num: 0,
        };
        lexer.advance();
        lexer
    }

    fn advance(&mut self) {
        if let Some((bp, ch)) = self.source_itr.next() {
            if ch == '\n' {
                self.line_num += 1;
                self.col_num = 0;
            } else {
                self.col_num += 1;
            }
            self.ch = Some((bp, ch))
        } else {
            self.ch = None;
        }
    }

    pub fn next_token(&mut self) -> Result<Token<'a>, Box<dyn Diagnostic>> {
        self.skip_whitespaces();
        let (_, ch) = self.ch.unwrap_or((self.source.len(), '\0'));
        match ch {
            c if c.is_alphabetic() || c.eq(&'_') => self.read_word(),
            c if c.is_digit(10) => self.read_number(),
            ':' | '=' | '>' | '<' | '!' | '&' | '|' | '*' | '/' | '+' | '-' | '.' | '%' => {
                self.read_operator()
            }
            ';' | ',' | '(' | ')' | '[' | ']' | '{' | '}' => self.read_punctuation(),
            '"' => self.read_string(),
            '\'' => self.read_char(),
            '\0' => Ok(self.read_eof().unwrap()),
            _ => self.read_illegal(),
        }
    }

    fn peek_char(&mut self) -> Option<(usize, char)> {
        self.source_itr.peek().copied()
    }

    fn skip_whitespaces(&mut self) {
        while let Some((_, ch)) = self.ch {
            if !ch.is_whitespace() {
                break;
            }
            self.advance();
        }
    }

    fn read_operator(&mut self) -> Result<Token<'a>, Box<dyn Diagnostic>> {
        let (start_bp, ch) = self.ch.unwrap();
        let line_start = self.line_num;
        let col_start = self.col_num;
        let token_type = match ch {
            ':' => self
                .read_two_char_operator('=', TokenType::Assign) // TODO check if rust has varags,
                // to send multiple token types
                .unwrap_or(TokenType::Illegal),
            '=' => self
                .read_two_char_operator('=', TokenType::EqEq)
                .unwrap_or(TokenType::Illegal),
            '>' => self
                .read_two_char_operator('=', TokenType::GtEq)
                .unwrap_or(TokenType::Gt),
            '<' => self
                .read_two_char_operator('=', TokenType::LtEq)
                .unwrap_or(TokenType::Lt),
            '!' => self
                .read_two_char_operator('=', TokenType::NotEq)
                .unwrap_or(TokenType::Not),
            '&' => self
                .read_two_char_operator('&', TokenType::And)
                .unwrap_or(TokenType::Illegal),
            '|' => self
                .read_two_char_operator('|', TokenType::Or)
                .unwrap_or(TokenType::Illegal),
            '+' => self
                .read_two_char_operator('+', TokenType::Inc)
                .unwrap_or(TokenType::Plus),
            '-' => self
                .read_two_char_operator('-', TokenType::Dec)
                .unwrap_or(TokenType::Minus),
            '.' => TokenType::Dot,
            '*' => TokenType::Asterix,
            '/' => TokenType::Slash,
            '%' => TokenType::Mod,
            _ => TokenType::Illegal,
        };
        self.advance();
        let (end_bp, _) = self.ch.unwrap_or((self.source.len(), '\0'));
        if token_type == TokenType::Illegal {
            Err(Box::new(LexerError::InvalidOperator(
                ch,
                Span {
                    start_byte_pos: start_bp,
                    end_byte_pos: end_bp,
                    line_num: line_start,
                    col_num: col_start,
                },
            )))
        } else {
            Ok(Token::new(
                token_type,
                &self.source[start_bp..end_bp],
                start_bp,
                end_bp,
                line_start,
                col_start,
            ))
        }
    }

    fn read_two_char_operator(
        &mut self,
        expected_char: char,
        expected_type: TokenType,
    ) -> Option<TokenType> {
        if self.peek_char().is_some_and(|(_, ch)| ch == expected_char) {
            self.advance();
            Some(expected_type)
        } else {
            None
        }
    }

    fn read_punctuation(&mut self) -> Result<Token<'a>, Box<dyn Diagnostic>> {
        let (start_bp, ch) = self.ch.unwrap();
        let line_start = self.line_num;
        let col_start = self.col_num;
        let token_type = match ch {
            ';' => TokenType::Semicolon,
            ',' => TokenType::Comma,
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '[' => TokenType::LSqBracket,
            ']' => TokenType::RSqBracket,
            '{' => TokenType::LBrace,
            '}' => TokenType::RBrace,
            _ => TokenType::Illegal,
        };
        self.advance();
        let (end_bp, _) = self.ch.unwrap_or((self.source.len(), '\0'));
        if token_type == TokenType::Illegal {
            Err(Box::new(LexerError::IllegalCharacter(
                ch,
                Span {
                    start_byte_pos: start_bp,
                    end_byte_pos: end_bp,
                    line_num: line_start,
                    col_num: col_start,
                },
            )))
        } else {
            Ok(Token::new(
                token_type,
                &self.source[start_bp..end_bp],
                start_bp,
                end_bp,
                line_start,
                col_start,
            ))
        }
    }

    fn read_string(&mut self) -> Result<Token<'a>, Box<dyn Diagnostic>> {
        self.advance(); // skip starting quote
        let (start_bp, _) = self.ch.unwrap();
        let start_line = self.line_num;
        let start_col = self.col_num;
        let mut is_terminated = false;
        while let Some((_, ch)) = self.ch {
            if ch == '"' {
                is_terminated = true;
                break;
            } else if ch == '\\' {
                self.advance(); // skip  the \
            }
            self.advance();
        }

        let (end_bp, _) = self.ch.unwrap_or((self.source.len(), '\0'));
        self.advance(); // skip the ending quote
        if is_terminated {
            Ok(Token::new(
                TokenType::String,
                &self.source[start_bp..end_bp],
                start_bp,
                end_bp,
                start_line,
                start_col,
            ))
        } else {
            Err(Box::new(LexerError::UnterminatedString(Span {
                start_byte_pos: start_bp,
                end_byte_pos: end_bp,
                line_num: start_line,
                col_num: start_col,
            })))
        }
    }

    fn read_char(&mut self) -> Result<Token<'a>, Box<dyn Diagnostic>> {
        self.advance(); // skip starting quote
        let (start_bp, _) = self.ch.unwrap();
        let start_line = self.line_num;
        let start_col = self.col_num;
        let mut is_terminated = false;
        while let Some((_, ch)) = self.ch {
            if ch == '\'' {
                is_terminated = true;
                break;
            } else if ch == '\\' {
                self.advance();
            }
            self.advance();
        }

        let (end_bp, _) = self.ch.unwrap_or((self.source.len(), '\0'));
        self.advance(); // skip the ending quote
        if is_terminated {
            Ok(Token::new(
                TokenType::Char,
                &self.source[start_bp..end_bp],
                start_bp,
                end_bp,
                start_line,
                start_col,
            ))
        } else {
            Err(Box::new(LexerError::UnterminatedChar(Span {
                start_byte_pos: start_bp,
                end_byte_pos: end_bp,
                line_num: start_line,
                col_num: start_col,
            })))
        }
    }

    fn read_number(&mut self) -> Result<Token<'a>, Box<dyn Diagnostic>> {
        let (start_bp, _) = self.ch.unwrap();
        let start_line = self.line_num;
        let start_col = self.col_num;
        let mut is_floating_point = false;
        while let Some((_, ch)) = self.ch {
            match ch {
                '.' => {
                    if !is_floating_point {
                        is_floating_point = true;
                    } else {
                        while self.ch.is_some_and(|(_, ch)| {
                            ch.is_digit(10)
                                || ch == '_'
                                || ch == '.'
                                || ch == 'e'
                                || ch == 'E'
                                || ch == '+'
                                || ch == '-'
                        }) {
                            self.advance();
                        }
                        let (end_bp, _) = self.ch.unwrap_or((self.source.len(), '\0'));
                        return Err(Box::new(LexerError::InvalidDecimalPoint(
                            self.source[start_bp..end_bp].to_string(),
                            Span {
                                start_byte_pos: start_bp,
                                end_byte_pos: end_bp,
                                line_num: start_line,
                                col_num: start_col,
                            },
                        )));
                    }
                }
                '_' | 'e' | 'E' | '+' | '-' => {}
                c if c.is_digit(10) => {}
                _ => break,
            }
            self.advance();
        }

        let (end_bp, _) = self.ch.unwrap_or((self.source.len(), '\0'));
        Ok(Token::new(
            TokenType::Number,
            &self.source[start_bp..end_bp],
            start_bp,
            end_bp,
            start_line,
            start_col,
        ))
    }

    fn read_word(&mut self) -> Result<Token<'a>, Box<dyn Diagnostic>> {
        let (start_bp, _) = self.ch.unwrap();
        let start_line = self.line_num;
        let start_col = self.col_num;
        while self
            .ch
            .is_some_and(|(_, ch)| ch.is_alphanumeric() || ch == '_')
        {
            self.advance();
        }
        let end_bp = self.ch.map(|(bp, _)| bp).unwrap_or(self.source.len());
        let token_type = match &self.source[start_bp..end_bp] {
            "let" => TokenType::Let,
            "fn" => TokenType::Function,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "typeof" => TokenType::TypeOf,
            _ => TokenType::Identifier,
        };
        Ok(Token::new(
            token_type,
            &self.source[start_bp..end_bp],
            start_bp,
            end_bp,
            start_line,
            start_col,
        ))
    }

    fn read_eof(&mut self) -> Option<Token<'a>> {
        if self.ch.is_none() {
            Some(Token::new(
                TokenType::Eof,
                "\0",
                self.source.len(),
                self.source.len(),
                self.line_num,
                self.col_num,
            ))
        } else {
            None
        }
    }

    fn read_illegal(&mut self) -> Result<Token<'a>, Box<dyn Diagnostic>> {
        let (bp, ch) = self.ch.unwrap();
        Err(Box::new(LexerError::IllegalCharacter(
            ch,
            Span {
                start_byte_pos: bp,
                end_byte_pos: bp + 1,
                line_num: self.line_num,
                col_num: self.col_num,
            },
        )))
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{Lexer, TokenType};

    #[test]
    fn test_lexer_read_operator() {
        let source = ":===+-/*!=>>=<<=!&&||";
        let mut lexer = Lexer::new(&source);
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Assign);
                assert_eq!(token.lexeme, ":=");
                assert_eq!(token.span.start_byte_pos, 0);
                assert_eq!(token.span.end_byte_pos, 2);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 1);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::EqEq);
                assert_eq!(token.lexeme, "==");
                assert_eq!(token.span.start_byte_pos, 2);
                assert_eq!(token.span.end_byte_pos, 4);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 3);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Plus);
                assert_eq!(token.lexeme, "+");
                assert_eq!(token.span.start_byte_pos, 4);
                assert_eq!(token.span.end_byte_pos, 5);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 5);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Minus);
                assert_eq!(token.lexeme, "-");
                assert_eq!(token.span.start_byte_pos, 5);
                assert_eq!(token.span.end_byte_pos, 6);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 6);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Slash);
                assert_eq!(token.lexeme, "/");
                assert_eq!(token.span.start_byte_pos, 6);
                assert_eq!(token.span.end_byte_pos, 7);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 7);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Asterix);
                assert_eq!(token.lexeme, "*");
                assert_eq!(token.span.start_byte_pos, 7);
                assert_eq!(token.span.end_byte_pos, 8);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 8);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        // let source = ":===+-/*!=>>=<<=!&&||";
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::NotEq);
                assert_eq!(token.lexeme, "!=");
                assert_eq!(token.span.start_byte_pos, 8);
                assert_eq!(token.span.end_byte_pos, 10);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 9);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Gt);
                assert_eq!(token.lexeme, ">");
                assert_eq!(token.span.start_byte_pos, 10);
                assert_eq!(token.span.end_byte_pos, 11);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 11);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::GtEq);
                assert_eq!(token.lexeme, ">=");
                assert_eq!(token.span.start_byte_pos, 11);
                assert_eq!(token.span.end_byte_pos, 13);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 12);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Lt);
                assert_eq!(token.lexeme, "<");
                assert_eq!(token.span.start_byte_pos, 13);
                assert_eq!(token.span.end_byte_pos, 14);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 14);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::LtEq);
                assert_eq!(token.lexeme, "<=");
                assert_eq!(token.span.start_byte_pos, 14);
                assert_eq!(token.span.end_byte_pos, 16);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 15);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        // let source = ":===+-/*!=>>=<<=!&&||";
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Not);
                assert_eq!(token.lexeme, "!");
                assert_eq!(token.span.start_byte_pos, 16);
                assert_eq!(token.span.end_byte_pos, 17);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 17);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        // let source = ":===+-/*!=>>=<<=!&&||";
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::And);
                assert_eq!(token.lexeme, "&&");
                assert_eq!(token.span.start_byte_pos, 17);
                assert_eq!(token.span.end_byte_pos, 19);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 18);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        // let source = ":===+-/*!=>>=<<=!&&||";
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Or);
                assert_eq!(token.lexeme, "||");
                assert_eq!(token.span.start_byte_pos, 19);
                assert_eq!(token.span.end_byte_pos, 21);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 20);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };
    }

    #[test]
    fn test_lexer_read_punctuation() {
        // ';' | ',' | '(' | ')' | '[' | ']' | '{' | '}'
        let source = ";,()[]{}";
        let mut lexer = Lexer::new(&source);
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Semicolon);
                assert_eq!(token.lexeme, ";");
                assert_eq!(token.span.start_byte_pos, 0);
                assert_eq!(token.span.end_byte_pos, 1);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 1);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        // ';' | ',' | '(' | ')' | '[' | ']' | '{' | '}'
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Comma);
                assert_eq!(token.lexeme, ",");
                assert_eq!(token.span.start_byte_pos, 1);
                assert_eq!(token.span.end_byte_pos, 2);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 2);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        // ';' | ',' | '(' | ')' | '[' | ']' | '{' | '}'
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::LParen);
                assert_eq!(token.lexeme, "(");
                assert_eq!(token.span.start_byte_pos, 2);
                assert_eq!(token.span.end_byte_pos, 3);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 3);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        // ';' | ',' | '(' | ')' | '[' | ']' | '{' | '}'
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::RParen);
                assert_eq!(token.lexeme, ")");
                assert_eq!(token.span.start_byte_pos, 3);
                assert_eq!(token.span.end_byte_pos, 4);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 4);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        // ';' | ',' | '(' | ')' | '[' | ']' | '{' | '}'
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::LSqBracket);
                assert_eq!(token.lexeme, "[");
                assert_eq!(token.span.start_byte_pos, 4);
                assert_eq!(token.span.end_byte_pos, 5);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 5);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        // ';' | ',' | '(' | ')' | '[' | ']' | '{' | '}'
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::RSqBracket);
                assert_eq!(token.lexeme, "]");
                assert_eq!(token.span.start_byte_pos, 5);
                assert_eq!(token.span.end_byte_pos, 6);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 6);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        // ';' | ',' | '(' | ')' | '[' | ']' | '{' | '}'
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::LBrace);
                assert_eq!(token.lexeme, "{");
                assert_eq!(token.span.start_byte_pos, 6);
                assert_eq!(token.span.end_byte_pos, 7);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 7);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        // ';' | ',' | '(' | ')' | '[' | ']' | '{' | '}'
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::RBrace);
                assert_eq!(token.lexeme, "}");
                assert_eq!(token.span.start_byte_pos, 7);
                assert_eq!(token.span.end_byte_pos, 8);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 8);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };
    }

    #[test]
    fn test_lexer_read_string() {
        let source = "\"Hello, World!\"";
        let mut lexer = Lexer::new(&source);
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::String);
                assert_eq!(token.lexeme, "Hello, World!");
                assert_eq!(token.span.start_byte_pos, 1);
                assert_eq!(token.span.end_byte_pos, 14);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 2);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let source = "\"Hello, \t🤗 World!\"";
        let mut lexer = Lexer::new(&source);
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::String);
                assert_eq!(token.span.start_byte_pos, 1);
                assert_eq!(token.lexeme, "Hello, \t🤗 World!");
                assert_eq!(token.span.end_byte_pos, 20);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 2);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let source = r#"" \" ""#;
        println!("source: {source}");
        let mut lexer = Lexer::new(&source);
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::String);
                assert_eq!(token.lexeme, " \\\" ");
                assert_eq!(token.span.start_byte_pos, 1);
                assert_eq!(token.span.end_byte_pos, 5);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 2);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };
    }

    #[test]
    fn test_lexer_read_char() {
        let source = "'a'";
        let mut lexer = Lexer::new(&source);
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Char);
                assert_eq!(token.lexeme, "a");
                assert_eq!(token.span.start_byte_pos, 1);
                assert_eq!(token.span.end_byte_pos, 2);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 2);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let source = "'\t'";
        let mut lexer = Lexer::new(&source);
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Char);
                assert_eq!(token.lexeme, "\t");
                assert_eq!(token.span.start_byte_pos, 1);
                assert_eq!(token.span.end_byte_pos, 2);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 2);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let source = "'🤗'";
        let mut lexer = Lexer::new(&source);
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Char);
                assert_eq!(token.lexeme, "🤗");
                assert_eq!(token.span.start_byte_pos, 1);
                assert_eq!(token.span.end_byte_pos, 5);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 2);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };
    }

    #[test]
    fn test_lexer_read_number() {
        let source = "10.00 1_000 1___000.00 123.45e-6 12002.00.00";
        let mut lexer = Lexer::new(&source);
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Number);
                assert_eq!(token.lexeme, "10.00");
                assert_eq!(token.span.start_byte_pos, 0);
                assert_eq!(token.span.end_byte_pos, 5);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 1);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Number);
                assert_eq!(token.lexeme, "1_000");
                assert_eq!(token.span.start_byte_pos, 6);
                assert_eq!(token.span.end_byte_pos, 11);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 7);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Number);
                assert_eq!(token.lexeme, "1___000.00");
                assert_eq!(token.span.start_byte_pos, 12);
                assert_eq!(token.span.end_byte_pos, 22);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 13);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Number);
                assert_eq!(token.lexeme, "123.45e-6");
                assert_eq!(token.span.start_byte_pos, 23);
                assert_eq!(token.span.end_byte_pos, 32);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 24);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                panic!("Has 2 decimal points {:?}", token)
            }
            Err(err) => {
                assert_eq!(
                    err.message(),
                    "number 12002.00.00 contains multiple decimal points"
                )
            }
        };
    }

    #[test]
    fn test_lexer_read_word() {
        let source = "if let fn false true else return temp_1";
        let mut lexer = Lexer::new(&source);
        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::If);
                assert_eq!(token.lexeme, "if");
                assert_eq!(token.span.start_byte_pos, 0);
                assert_eq!(token.span.end_byte_pos, 2);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 1);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Let);
                assert_eq!(token.lexeme, "let");
                assert_eq!(token.span.start_byte_pos, 3);
                assert_eq!(token.span.end_byte_pos, 6);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 4);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Function);
                assert_eq!(token.lexeme, "fn");
                assert_eq!(token.span.start_byte_pos, 7);
                assert_eq!(token.span.end_byte_pos, 9);
                assert_eq!(token.span.line_num, 1);
                assert_eq!(token.span.col_num, 8);
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::False);
                assert_eq!(token.lexeme, "false");
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::True);
                assert_eq!(token.lexeme, "true");
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Else);
                assert_eq!(token.lexeme, "else");
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Return);
                assert_eq!(token.lexeme, "return");
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = lexer.next_token();
        match t {
            Ok(token) => {
                assert_eq!(token.t_type, TokenType::Identifier);
                assert_eq!(token.lexeme, "temp_1");
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Eof);
    }

    #[test]
    fn test_lexer_read_source_code() {
        let source = "let x := 10_000;
        let y := 111_111;
        let add := fn (a, b) {
            return a + b;
        };
        print(add(x, y));
        if true != false {
            print(\"hello, world!\");
        } else {
            print(\"I don't feel like greeting\");
        }
        ";
        let mut lexer = Lexer::new(&source);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Let);
        assert_eq!(t.lexeme, "let");
        assert_eq!(t.span.line_num, 1);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "x");
        assert_eq!(t.span.line_num, 1);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Assign);
        assert_eq!(t.lexeme, ":=");
        assert_eq!(t.span.line_num, 1);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Number);
        assert_eq!(t.lexeme, "10_000");
        assert_eq!(t.span.line_num, 1);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Semicolon);
        assert_eq!(t.lexeme, ";");
        assert_eq!(t.span.line_num, 1);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Let);
        assert_eq!(t.lexeme, "let");
        assert_eq!(t.span.line_num, 2);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "y");
        assert_eq!(t.span.line_num, 2);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Assign);
        assert_eq!(t.lexeme, ":=");
        assert_eq!(t.span.line_num, 2);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Number);
        assert_eq!(t.lexeme, "111_111");
        assert_eq!(t.span.line_num, 2);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Semicolon);
        assert_eq!(t.lexeme, ";");
        assert_eq!(t.span.line_num, 2);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Let);
        assert_eq!(t.lexeme, "let");
        assert_eq!(t.span.line_num, 3);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "add");
        assert_eq!(t.span.line_num, 3);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Assign);
        assert_eq!(t.lexeme, ":=");
        assert_eq!(t.span.line_num, 3);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Function);
        assert_eq!(t.lexeme, "fn");
        assert_eq!(t.span.line_num, 3);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::LParen);
        assert_eq!(t.lexeme, "(");
        assert_eq!(t.span.line_num, 3);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "a");
        assert_eq!(t.span.line_num, 3);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Comma);
        assert_eq!(t.lexeme, ",");
        assert_eq!(t.span.line_num, 3);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "b");
        assert_eq!(t.span.line_num, 3);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::RParen);
        assert_eq!(t.lexeme, ")");
        assert_eq!(t.span.line_num, 3);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::LBrace);
        assert_eq!(t.lexeme, "{");
        assert_eq!(t.span.line_num, 3);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Return);
        assert_eq!(t.lexeme, "return");
        assert_eq!(t.span.line_num, 4);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "a");
        assert_eq!(t.span.line_num, 4);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Plus);
        assert_eq!(t.lexeme, "+");
        assert_eq!(t.span.line_num, 4);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "b");
        assert_eq!(t.span.line_num, 4);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Semicolon);
        assert_eq!(t.lexeme, ";");
        assert_eq!(t.span.line_num, 4);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::RBrace);
        assert_eq!(t.lexeme, "}");
        assert_eq!(t.span.line_num, 5);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Semicolon);
        assert_eq!(t.lexeme, ";");
        assert_eq!(t.span.line_num, 5);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "print");
        assert_eq!(t.span.line_num, 6);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::LParen);
        assert_eq!(t.lexeme, "(");
        assert_eq!(t.span.line_num, 6);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "add");
        assert_eq!(t.span.line_num, 6);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::LParen);
        assert_eq!(t.lexeme, "(");
        assert_eq!(t.span.line_num, 6);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "x");
        assert_eq!(t.span.line_num, 6);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Comma);
        assert_eq!(t.lexeme, ",");
        assert_eq!(t.span.line_num, 6);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "y");
        assert_eq!(t.span.line_num, 6);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::RParen);
        assert_eq!(t.lexeme, ")");
        assert_eq!(t.span.line_num, 6);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::RParen);
        assert_eq!(t.lexeme, ")");
        assert_eq!(t.span.line_num, 6);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Semicolon);
        assert_eq!(t.lexeme, ";");
        assert_eq!(t.span.line_num, 6);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::If);
        assert_eq!(t.lexeme, "if");
        assert_eq!(t.span.line_num, 7);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::True);
        assert_eq!(t.lexeme, "true");
        assert_eq!(t.span.line_num, 7);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::NotEq);
        assert_eq!(t.lexeme, "!=");
        assert_eq!(t.span.line_num, 7);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::False);
        assert_eq!(t.lexeme, "false");
        assert_eq!(t.span.line_num, 7);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::LBrace);
        assert_eq!(t.lexeme, "{");
        assert_eq!(t.span.line_num, 7);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "print");
        assert_eq!(t.span.line_num, 8);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::LParen);
        assert_eq!(t.lexeme, "(");
        assert_eq!(t.span.line_num, 8);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::String);
        assert_eq!(t.lexeme, "hello, world!");
        assert_eq!(t.span.line_num, 8);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::RParen);
        assert_eq!(t.lexeme, ")");
        assert_eq!(t.span.line_num, 8);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Semicolon);
        assert_eq!(t.lexeme, ";");
        assert_eq!(t.span.line_num, 8);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::RBrace);
        assert_eq!(t.lexeme, "}");
        assert_eq!(t.span.line_num, 9);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Else);
        assert_eq!(t.lexeme, "else");
        assert_eq!(t.span.line_num, 9);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::LBrace);
        assert_eq!(t.lexeme, "{");
        assert_eq!(t.span.line_num, 9);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Identifier);
        assert_eq!(t.lexeme, "print");
        assert_eq!(t.span.line_num, 10);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::LParen);
        assert_eq!(t.lexeme, "(");
        assert_eq!(t.span.line_num, 10);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::String);
        assert_eq!(t.lexeme, "I don't feel like greeting");
        assert_eq!(t.span.line_num, 10);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::RParen);
        assert_eq!(t.lexeme, ")");
        assert_eq!(t.span.line_num, 10);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Semicolon);
        assert_eq!(t.lexeme, ";");
        assert_eq!(t.span.line_num, 10);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::RBrace);
        assert_eq!(t.lexeme, "}");
        assert_eq!(t.span.line_num, 11);

        let t = match lexer.next_token() {
            Ok(token) => token,
            Err(err) => panic!("{}", err.message()),
        };
        assert_eq!(t.t_type, TokenType::Eof);
    }
}
