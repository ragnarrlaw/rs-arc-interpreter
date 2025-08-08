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
    InvalidEscapeSequence(String, Span),
}

impl Diagnostic for ParserError {
    fn message(&self) -> String {
        match self {
            Self::InvalidExponent(lexeme, _) => {
                format!("{lexeme} invalid exponent found")
            }
            Self::InvalidCharValue(lexeme, _) => {
                format!("invalid character value {lexeme} found")
            }
            Self::InvalidNumberValue(lexeme, _) => {
                format!("invalid number value {lexeme} found")
            }
            Self::UnexpectedToken {
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
            Self::MissingToken {
                missing,
                span: _,
                hint: _,
            } => {
                format!("missing {missing} token")
            }
            Self::InvalidEscapeSequence(seq, _) => {
                format!("Invalid escape sequence '{}' found", seq)
            }
        }
    }

    fn span(&self) -> Span {
        match self {
            Self::InvalidExponent(_, span) => *span,
            Self::InvalidCharValue(_, span) => *span,
            Self::InvalidNumberValue(_, span) => *span,
            Self::UnexpectedToken {
                expected: _,
                found: _,
                span,
                hint: _,
            } => *span,
            Self::MissingToken {
                missing: _,
                span,
                hint: _,
            } => *span,
            Self::InvalidEscapeSequence(_, span) => *span,
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
