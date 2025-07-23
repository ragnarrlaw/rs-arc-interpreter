pub mod token;

use crate::{
    error::{ErrorType, IntprError},
    lexer::token::{Token, TokenType},
    line_map::LineMap,
};
use std::{iter::Peekable, str::CharIndices};

#[derive(Debug)]
pub struct Lexer<'a> {
    pub source: &'a str,
    pub line_map: &'a LineMap,
    pub source_itr: Peekable<CharIndices<'a>>,
    pub ch: Option<(usize, char)>, // track the byte position of the current character and the character
    pub line_num: usize,
    pub col_num: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, line_map: &'a LineMap) -> Self {
        let mut lexer = Lexer {
            source,
            line_map,
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

    pub fn next_token(&mut self) -> Result<Token<'a>, IntprError> {
        self.skip_whitespaces();
        let (_, ch) = self.ch.unwrap_or((self.source.len(), '\0'));
        match ch {
            c if c.is_alphabetic() || c.eq(&'_') => self.read_word(),
            c if c.is_digit(10) => self.read_number(),
            ':' | '=' | '>' | '<' | '!' | '&' | '|' | '*' | '/' | '+' | '-' => self.read_operator(),
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

    fn read_operator(&mut self) -> Result<Token<'a>, IntprError> {
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
            '*' => TokenType::Asterix,
            '/' => TokenType::Slash,
            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            _ => TokenType::Illegal,
        };
        self.advance();
        let (end_bp, _) = self.ch.unwrap_or((self.source.len(), '\0'));
        if token_type == TokenType::Illegal {
            Err(IntprError::new(
                self.source,
                self.line_map,
                ErrorType::LexerErrorInvalidOperator,
                &self.source[start_bp..end_bp],
                start_bp,
                end_bp,
                line_start,
                col_start,
            ))
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

    fn read_punctuation(&mut self) -> Result<Token<'a>, IntprError> {
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
            Err(IntprError::new(
                self.source,
                self.line_map,
                ErrorType::LexerErrorIllegalCharacter,
                &self.source[start_bp..end_bp],
                start_bp,
                end_bp,
                line_start,
                col_start,
            ))
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

    fn read_string(&mut self) -> Result<Token<'a>, IntprError> {
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
                self.advance();
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
            Err(IntprError::new(
                self.source,
                self.line_map,
                ErrorType::LexerErrorUnterminatedString,
                &self.source[start_bp..end_bp],
                start_bp,
                end_bp,
                start_line,
                start_col,
            ))
        }
    }

    fn read_char(&mut self) -> Result<Token<'a>, IntprError> {
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
            Err(IntprError::new(
                self.source,
                self.line_map,
                ErrorType::LexerErrorUnterminatedChar,
                &self.source[start_bp..end_bp],
                start_bp,
                end_bp,
                start_line,
                start_col,
            ))
        }
    }

    fn read_number(&mut self) -> Result<Token<'a>, IntprError> {
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
                        while self
                            .ch
                            .is_some_and(|(_, ch)| ch.is_digit(10) || ch == '_' || ch == '.')
                        {
                            self.advance();
                        }
                        let (end_bp, _) = self.ch.unwrap_or((self.source.len(), '\0'));
                        return Err(IntprError::new(
                            self.source,
                            self.line_map,
                            ErrorType::LexerErrorInvalidDecimalPoint,
                            &self.source[start_bp..end_bp],
                            start_bp,
                            end_bp,
                            start_line,
                            start_col,
                        ));
                    }
                }
                '_' => {}
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

    fn read_word(&mut self) -> Result<Token<'a>, IntprError> {
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

    fn read_illegal(&mut self) -> Result<Token<'a>, IntprError> {
        let bp = self.ch.map(|(bp, _)| bp).unwrap();
        Err(IntprError::new(
            self.source,
            self.line_map,
            ErrorType::LexerErrorIllegalCharacter,
            &self.source[bp..=bp],
            bp,
            bp,
            self.line_num,
            self.col_num,
        ))
    }
}
