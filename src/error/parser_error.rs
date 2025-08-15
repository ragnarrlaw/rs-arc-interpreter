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
    InvalidUseOfReturnStatement {
        case: String,
        span: Span,
        hint: Option<String>,
    },
    InvalidUseOfExpressionStatement {
        case: String,
        span: Span,
        hint: Option<String>,
    },
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
                expected, found, ..
            } => {
                if expected.is_empty() {
                    format!("unexpected {found} token found")
                } else {
                    format!("expected {expected}, but found {found}")
                }
            }
            Self::MissingToken { missing, .. } => {
                format!("missing {missing} token")
            }
            Self::InvalidEscapeSequence(seq, _) => {
                format!("Invalid escape sequence '{seq}' found")
            }
            Self::InvalidUseOfReturnStatement { case, .. } => {
                format!("invalid use of return statement found '{case}'")
            }
            Self::InvalidUseOfExpressionStatement { case, .. } => {
                format!("invalid use of an expression {case}")
            }
        }
    }

    fn span(&self) -> Span {
        match self {
            Self::InvalidExponent(_, span) => *span,
            Self::InvalidCharValue(_, span) => *span,
            Self::InvalidNumberValue(_, span) => *span,
            Self::UnexpectedToken { span, .. } => *span,
            Self::MissingToken { span, .. } => *span,
            Self::InvalidEscapeSequence(_, span) => *span,
            Self::InvalidUseOfReturnStatement { span, .. } => *span,
            Self::InvalidUseOfExpressionStatement { span, .. } => *span,
        }
    }

    fn help(&self) -> Option<String> {
        match self {
            Self::UnexpectedToken { hint, .. } => hint.clone(),
            Self::MissingToken { hint, .. } => hint.clone(),
            Self::InvalidUseOfReturnStatement { hint, .. } => hint.clone(),
            Self::InvalidUseOfExpressionStatement { hint, .. } => hint.clone(),
            _ => None,
        }
    }
}
