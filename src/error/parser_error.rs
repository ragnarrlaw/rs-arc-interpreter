use crate::{error::diagnostic::Diagnostic, lexer::span::Span};

#[derive(Debug, PartialEq)]
pub enum ParserError {
    InvalidExponent(String, Span), // a number written in scientific notation has an invalid format
    InvalidCharValue(String, Span),
    InvalidNumberValue(String, Span),
    UnexpectedToken {
        expected: String,
        found: String,
        span: Span,
        hint: Option<String>,
    },
    MissingToken {
        missing: String,
        span: Span,
        hint: Option<String>,
    },
}

impl Diagnostic for ParserError {
    fn message(&self) -> String {
        match self {
            ParserError::InvalidExponent(lexeme, _) => {
                format!("{lexeme} invalid exponent found")
            }
            ParserError::InvalidCharValue(lexeme, _) => {
                format!("invalid character value {lexeme} found")
            }
            ParserError::InvalidNumberValue(lexeme, _) => {
                format!("invalid number value {lexeme} found")
            }
            ParserError::UnexpectedToken {
                expected,
                found,
                span: _,
                hint: _,
            } => {
                if expected.is_empty() {
                    format!("unexpected {found} token found")
                } else {
                    format!("expected {expected}, but found {found}")
                }
            }
            ParserError::MissingToken {
                missing,
                span: _,
                hint: _,
            } => {
                format!("missing {missing} token")
            }
        }
    }

    fn span(&self) -> Span {
        match self {
            ParserError::InvalidExponent(_, span) => *span,
            ParserError::InvalidCharValue(_, span) => *span,
            ParserError::InvalidNumberValue(_, span) => *span,
            ParserError::UnexpectedToken {
                expected: _,
                found: _,
                span,
                hint: _,
            } => *span,
            ParserError::MissingToken {
                missing: _,
                span,
                hint: _,
            } => *span,
        }
    }

    fn help(&self) -> Option<String> {
        match self {
            ParserError::UnexpectedToken {
                expected: _,
                found: _,
                span: _,
                hint,
            } => hint.clone(),
            ParserError::MissingToken {
                missing: _,
                span: _,
                hint,
            } => hint.clone(),
            _ => None,
        }
    }
}
