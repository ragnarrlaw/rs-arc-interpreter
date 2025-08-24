use crate::error::context::Context;
use core::fmt;
use std::fmt::Display;

use crate::lexer::span::Span;

pub trait Diagnostic: fmt::Debug {
    fn context(&self) -> Option<Context> {
        None
    }

    fn message(&self) -> String;

    fn span(&self) -> Span;

    // optional helper message
    fn help(&self) -> Option<String> {
        None
    }
}

impl Display for dyn Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}
