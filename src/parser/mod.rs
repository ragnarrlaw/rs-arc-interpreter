use crate::{
    error::{diagnostic::Diagnostic},
    lexer::{
        span::Span, token::{ Token, TokenType}, Lexer
    },
    line_map::LineMap,
    parser::ast::{Expression, Program, Statement},
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
    ) -> Result<Self, Box<dyn Diagnostic>> {
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

    fn advance(&mut self) -> Result<(), Box<dyn Diagnostic>> {
        let t = self.lexer.next_token()?;
        self.curr_token = self.peek_token.take();
        self.peek_token = Some(t);
        Ok(())
    }

    pub fn parse_program(&mut self) -> Result<ast::Program<'a>, Box<dyn Diagnostic>> {
        let mut stmts: Vec<Statement<'a>> = vec![];
        while self
            .curr_token
            .as_ref()
            .is_some_and(|t| t.t_type != TokenType::Eof)
        {
            stmts.push(self.parse_statement()?);
            self.advance()?;
        }
        Ok(Program { statements: stmts })
    }

    fn parse_statement(&mut self) -> Result<ast::Statement<'a>, Box<dyn Diagnostic>> {
        let token = self.curr_token.as_ref().unwrap();
        match token.t_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::Function => self.parse_fn_definition(),
            _ => self.parse_expression_statement(),
        }
    }

    /*
     * let := 10;
     * let x 10;
     * */
    fn parse_let_statement(&mut self) -> Result<ast::Statement<'a>, Box<dyn Diagnostic>> {
        todo!()
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement<'a>, Box<dyn Diagnostic>> {
        todo!()
    }

    /*
     * expressions that appear at the hightest level
     *
     * >> 5
     * >> 10 + 10
     * >> 10
     * >> "string"
     * >> {
     * >>   let x = 10;
     * >>   x + 20
     * >> }
     *
     * */
    fn parse_expression_statement(&mut self) -> Result<ast::Statement<'a>, Box<dyn Diagnostic>> {
        todo!()
    }

    /*
     * fn function_definition(param_1, param_2, param_3) {
     *    // code
     * }
     * */
    fn parse_fn_definition(&mut self) -> Result<ast::Statement<'a>, Box<dyn Diagnostic>> {
        todo!()
    }

    fn parse_expression(&mut self) -> Result<ast::Expression<'a>, Box<dyn Diagnostic>> {
        todo!()
    }

    fn curr_token_is(&self, t_type: TokenType) -> bool {
        self.curr_token.as_ref().is_some_and(|t| t.t_type == t_type)
    }

    fn peek_token_is(&self, t_type: TokenType) -> bool {
        self.peek_token.as_ref().is_some_and(|t| t.t_type == t_type)
    }

    fn jump_to_sync_point(&mut self, sync_tokens: Vec<TokenType>) {
        while self
            .curr_token
            .as_ref()
            .is_some_and(|t| t.t_type != TokenType::Eof)
        {
            if self
                .curr_token
                .as_ref()
                .is_some_and(|t| sync_tokens.contains(&t.t_type))
            {
                break;
            }
        }
    }

    // fn function_def (param1, param2, param3)
    //                  ^                    ^
    // read paramter alists
    fn read_function_params(&mut self) -> Result<Vec<String>, Box<dyn Diagnostic>> {
        todo!()
    }

    // call(arg1, arg2, arg3)
    //      ^              ^
    // read function argument list
    fn read_function_args(&mut self) -> Result<Vec<Expression<'a>>, Box<dyn Diagnostic>> {
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
        let mut lexer = Lexer::new(source);

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
                panic!("{}", err.message())
            }
        };
    }

    #[test]
    fn test_parse_return_statements() {
        let source = "return 10;
        return x; 
        return add(10, 20);";

        let line_map = LineMap::new(source);
        let mut lexer = Lexer::new(source);

        let parser_res = Parser::new(source, &mut lexer, &line_map);
        match parser_res {
            Ok(mut parser) => {
                match parser.parse_program() {
                    Ok(program) => {
                        assert_eq!(program.statements.len(), 3);
                    }
                    Err(_err) => {
                        panic!("parser failed to identify the let statements")
                    }
                };
            }
            Err(err) => {
                panic!("{}", err.message())
            }
        };
    }
}
