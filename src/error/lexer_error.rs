use crate::error::diagnostic::Diagnostic;
use crate::lexer::span::Span;

#[derive(Debug, PartialEq)]
pub enum LexerError {
    IllegalCharacter(char, Span),
    InvalidOperator(char, Span),
    UnterminatedString(Span),
    UnterminatedChar(Span),
    InvalidEscapeSequence(String, Span),
    InvalidDecimalPoint(String, Span),
}

impl Diagnostic for LexerError {
    fn message(&self) -> String {
        match self {
            LexerError::IllegalCharacter(character, _) => {
                format!("invalid {character} found")
            }
            LexerError::InvalidOperator(operator, _) => {
                format!("invalid operator {operator} found")
            }
            LexerError::UnterminatedString(_) => {
                format!("unterminated string found")
            }
            LexerError::UnterminatedChar(_) => {
                format!("unterminated character found")
            }
            LexerError::InvalidEscapeSequence(lexeme, _) => {
                format!("invalid escape sequence {lexeme} found")
            }
            LexerError::InvalidDecimalPoint(lexeme, _) => {
                format!("number {lexeme} contains multiple decimal points")
            }
        }
    }

    fn span(&self) -> Span {
        match self {
            LexerError::IllegalCharacter(_, span) => *span,
            LexerError::InvalidOperator(_, span) => *span,
            LexerError::UnterminatedString(span) => *span,
            LexerError::UnterminatedChar(span) => *span,
            LexerError::InvalidEscapeSequence(_, span) => *span,
            LexerError::InvalidDecimalPoint(_, span) => *span,
        }
    }
}
