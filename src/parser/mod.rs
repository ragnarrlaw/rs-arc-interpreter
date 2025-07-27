use crate::{
    error::IntprError,
    lexer::{Lexer, token::Token},
    line_map::LineMap,
};

pub mod ast;

#[derive(Debug)]
struct Parser<'a> {
    source: &'a str,
    lexer: Lexer<'a>,
    line_map: &'a LineMap,
    curr_token: Option<Token<'a>>,
    peek_token: Option<Token<'a>>,
    errors: Vec<IntprError<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, lexer: Lexer<'a>, line_map: &'a LineMap) -> Self {
        Parser {
            source,
            lexer,
            line_map,
            curr_token: None,
            peek_token: None,
            errors: vec![],
        }
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.take();
        self.peek_token = match self.lexer.next_token() {
            Ok(token) => Some(token),
            Err(err) => {
                self.errors.push(err); // TODO from here onwards skip to a save sequence ; or } some statement or expression termination condition
                None
            }
        };
    }

    pub fn parse_program(&mut self) -> Result<ast::Program<'a>, IntprError<'a>> {
        todo!()
    }

    fn parse_statement(&mut self) -> Result<ast::Statement<'a>, IntprError<'a>> {
        todo!()
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement<'a>, IntprError<'a>> {
        todo!()
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement<'a>, IntprError<'a>> {
        todo!()
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Statement<'a>, IntprError<'a>> {
        todo!()
    }

    fn parse_fn_definition(&mut self) -> Result<ast::Statement<'a>, IntprError<'a>> {
        todo!()
    }

    fn parse_expression(&mut self) -> Result<ast::Expression<'a>, IntprError<'a>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_parser() {}
}
