use core::fmt;

use crate::lexer::span::Span;

pub trait Diagnostic: fmt::Debug {
    fn message(&self) -> String;

    fn span(&self) -> Span;

    // optional helper message
    fn help(&self) -> Option<String> {
        None
    }
}
