use crate::{
    error::IntprError,
    lexer::{Lexer, token::Token},
    line_map::LineMap,
};

pub mod ast;

#[derive(Debug)]
struct Parser<'a> {
    source: &'a str,
    lexer: &'a mut Lexer<'a>,
    line_map: &'a LineMap,
    curr_token: Option<Token<'a>>,
    peek_token: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(
        source: &'a str,
        lexer: &'a mut Lexer<'a>,
        line_map: &'a LineMap,
    ) -> Result<Self, IntprError<'a>> {
        let mut parser = Parser {
            source,
            lexer,
            line_map,
            curr_token: None,
            peek_token: None,
        };
        parser.advance()?;
        parser.advance()?;
        Ok(parser)
    }

    fn advance(&mut self) -> Result<(), IntprError<'a>> {
        let t = self.lexer.next_token()?;
        self.curr_token = self.peek_token.take();
        self.peek_token = Some(t);
        Ok(())
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
    use crate::{
        lexer::Lexer,
        line_map::LineMap,
        parser::{Parser, ast::Statement},
    };

    #[test]
    fn test_parse_let_statement() {
        let source = "let x := 10;
        let y := 20; 
        let foobar := 8383;";

        let line_map = LineMap::new(source);
        let mut lexer = Lexer::new(source, &line_map);

        let parser_res = Parser::new(source, &mut lexer, &line_map);
        match parser_res {
            Ok(mut parser) => {
                match parser.parse_program() {
                    Ok(program) => {
                        assert_eq!(program.statements.len(), 3);
                        match &program.statements[0] {
                            Statement::Let {
                                span: _,
                                identifier,
                                value: _,
                            } => {
                                assert_eq!(identifier, "x");
                            }
                            _ => panic!("invalid statement"),
                        };
                        match &program.statements[1] {
                            Statement::Let {
                                span: _,
                                identifier,
                                value: _,
                            } => {
                                assert_eq!(identifier, "y");
                            }
                            _ => panic!("invalid statement"),
                        };

                        match &program.statements[2] {
                            Statement::Let {
                                span: _,
                                identifier,
                                value: _,
                            } => {
                                assert_eq!(identifier, "foobar");
                            }
                            _ => panic!("invalid statement"),
                        }
                    }
                    Err(_err) => {
                        panic!("parser failed to identify the let statements")
                    }
                };
            }
            Err(err) => {
                panic!("{err}")
            }
        };
    }
}
