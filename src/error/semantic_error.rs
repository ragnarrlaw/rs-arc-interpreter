use crate::error::context::Context;

use crate::{error::diagnostic::Diagnostic, lexer::span::Span};

#[derive(Debug)]
pub struct SemanticError {
    err_type: SemanticErrorType,
    span: Span,
    context: Option<Context>,
    help: Option<String>,
}

#[derive(Debug, PartialEq)]
pub enum SemanticErrorType {
    RedeclarationOfSameSymbol(String),
}

impl Diagnostic for SemanticError {
    fn message(&self) -> String {
        match &self.err_type {
            SemanticErrorType::RedeclarationOfSameSymbol(name) => {
                format!("variable with the same name '{name}' exisits")
            }
        }
    }

    fn help(&self) -> Option<String> {
        self.help.clone()
    }

    fn span(&self) -> Span {
        self.span
    }
}

impl SemanticError {
    pub fn new(
        context: Option<Context>,
        err_type: SemanticErrorType,
        span: Span,
        help: Option<String>,
    ) -> Self {
        Self {
            context,
            err_type,
            span,
            help,
        }
    }
}
