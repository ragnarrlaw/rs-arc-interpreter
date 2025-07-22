use crate::lexer::token::Span;
use crate::line_map::LineMap;
use std::error::Error;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum ErrorType {
    // lexer errors
    LexerErrorIllegalCharacter,
    LexerErrorInvalidOperator,
    LexerErrorUnterminatedString,
    LexerErrorUnterminatedChar,
    LexerErrorInvalidEscape,
    LexerErrorInvalidDecimalPoint,

    // parser errors
    ParserErrorInvalidExponent,
    ParserErrorInvalidChar,
    ParserErrorInvalidNumber,
}

#[derive(Debug)]
pub struct IntprError<'a> {
    pub source: &'a str,
    pub error_type: ErrorType,
    pub lexeme: &'a str,
    pub span: Span,
    pub msg: String,
}

impl<'a> Display for IntprError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl<'a> Error for IntprError<'a> {}

impl<'a> IntprError<'a> {
    pub fn new(
        source: &'a str,
        line_map: &'a LineMap,
        error_type: ErrorType,
        lexeme: &'a str,
        start_bp: usize,
        end_bp: usize,
        line_num: usize,
        col_num: usize,
    ) -> Self {
        let msg = IntprError::create_msg(
            source,
            &error_type,
            line_map,
            lexeme,
            start_bp,
            end_bp,
            line_num,
            col_num,
        );
        IntprError {
            source,
            error_type,
            lexeme,
            span: Span {
                start_byte_pos: start_bp,
                end_byte_pos: end_bp,
                line_num,
                col_num,
            },
            msg,
        }
    }

    fn create_msg(
        source: &'a str,
        error_type: &ErrorType,
        line_map: &'a LineMap,
        lexeme: &'a str,
        start_bp: usize,
        end_bp: usize,
        line_num: usize,
        col_num: usize,
    ) -> String {
        let (line_start, col) = line_map.get_position(start_bp).unwrap();
        match error_type {
            ErrorType::LexerErrorIllegalCharacter => {
                let s: String;
                s = format!("error[lexer]: invalid character {} found\n\t--><repl>:{}:{}\n\t|\n{} | {}\n\t|\n",
                   lexeme,
                   line_num,
                   col_num,
                   line_num,
                   "",
                );
                s
            }
            ErrorType::LexerErrorInvalidOperator => todo!(),
            ErrorType::LexerErrorUnterminatedString => todo!(),
            ErrorType::LexerErrorUnterminatedChar => todo!(),
            ErrorType::LexerErrorInvalidEscape => todo!(),
            ErrorType::LexerErrorInvalidDecimalPoint => todo!(),
            _ => format!(""),
        }
    }
}
