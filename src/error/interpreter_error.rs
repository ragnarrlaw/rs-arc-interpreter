use crate::error::diagnostic::Diagnostic;
use crate::error::lexer_error::LexerError;
use crate::error::parser_error::ParserError;

#[derive(Debug, PartialEq)]
pub enum InterpreterError {
    Lexer(LexerError),
    Parser(ParserError),
    // add TypeChecker(TypeError), and etc
}

impl Diagnostic for InterpreterError {
    fn message(&self) -> String {
        match self {
            InterpreterError::Lexer(err) => err.message(),
            InterpreterError::Parser(err) => err.message(),
        }
    }

    fn span(&self) -> crate::lexer::span::Span {
        match self {
            InterpreterError::Lexer(err) => err.span(),
            InterpreterError::Parser(err) => err.span(),
        }
    }

    fn help(&self) -> Option<String> {
        match self {
            InterpreterError::Lexer(err) => err.help(),
            InterpreterError::Parser(err) => err.help(),
        }
    }
}
