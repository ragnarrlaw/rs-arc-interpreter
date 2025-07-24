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
        let line_end = source[line_start..]
            .find('\n')
            .map(|i| line_start + i)
            .unwrap_or(source.len());
        let line_str = &source[line_start..line_end].trim_end_matches('\n');
        let pointer = " ".repeat(col) + &"^".repeat(end_bp - start_bp);
        let padding = " ".repeat(line_num.to_string().len());
        let file_name = "repl";
        let error_type = match error_type {
            ErrorType::LexerErrorIllegalCharacter => format!("invalid character {} found", lexeme),
            ErrorType::LexerErrorInvalidOperator => format!("invalid operator {} found", lexeme),
            ErrorType::LexerErrorUnterminatedString => {
                format!("unterminated string {} found", lexeme)
            }
            ErrorType::LexerErrorUnterminatedChar => {
                format!("unterminated character {} found", lexeme)
            }
            ErrorType::LexerErrorInvalidEscape => {
                format!("invalid escape sequence {} found", lexeme)
            }
            ErrorType::LexerErrorInvalidDecimalPoint => {
                format!("too many decimal points {} found", lexeme)
            }
            _ => panic!("Error type cannot be found"),
        };
        let s: String;
        s = format!(
            "error: {}.
{padding}--><{}>:{}:{}
{padding}|
{:>width$}| {}
{padding}| {}",
            error_type,
            file_name,
            line_num,
            col_num,
            line_num,
            line_str,
            pointer,
            width = line_num.to_string().len()
        );
        s
    }
}

#[cfg(test)]
mod test {
    use crate::{
        error::{ErrorType, IntprError},
        line_map::LineMap,
    };

    #[test]
    fn test_error() {
        let source = "let a = 10;\nlet f = fn() => {\"name\"}\n";
        let line_map = LineMap::new(&source);
        let err = IntprError::new(
            source,
            &line_map,
            ErrorType::LexerErrorIllegalCharacter,
            "let",
            0,
            3,
            1,
            1,
        );
        println!("{}", err);
        assert_eq!(err.error_type, ErrorType::LexerErrorIllegalCharacter);
        assert_eq!(
            err.msg,
            "error: invalid character let found.
 --><repl>:1:1
 |
1| let a = 10;
 | ^^^"
        );
    }
}
