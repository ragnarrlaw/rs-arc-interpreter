use std::thread::current;

use crate::{
    error::{diagnostic::Diagnostic, parser_error::ParserError},
    lexer::{
        Lexer,
        span::Span,
        token::{Token, TokenType},
    },
    parser::ast::{Expression, Program, Statement},
};

pub mod ast;

#[derive(Debug)]
struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    curr_token: Option<Token<'a>>,
    peek_token: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Result<Self, Box<dyn Diagnostic>> {
        let mut parser = Parser {
            lexer,
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
     * let x := 10;
     * */
    fn parse_let_statement(&mut self) -> Result<ast::Statement<'a>, Box<dyn Diagnostic>> {
        if !self.curr_token_is(TokenType::Let) {
            self.advance()?;
            return Err(Box::new(ParserError::UnexpectedToken {
                expected: "let".to_string(),
                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: Some(format!(
                    "let bindings start with \"let\" keyword. e.g. let x := 10;"
                )),
            }));
        }
        let start_span = self.curr_token.as_ref().unwrap().span;
        if !self.peek_token_is(TokenType::Identifier) {
            self.advance()?;
            return Err(Box::new(ParserError::UnexpectedToken {
                expected: "identifier".to_string(),
                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: None,
            }));
        }
        self.advance()?;
        let identifier = self.curr_token.as_ref().unwrap().lexeme.to_string();

        if !self.peek_token_is(TokenType::Assign) {
            self.advance()?;
            return Err(Box::new(ParserError::UnexpectedToken {
                expected: ":=".to_string(),
                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: None,
            }));
        }
        self.advance()?;
        self.advance()?;

        let expr = self.parse_expression()?;

        if !self.curr_token_is(TokenType::Semicolon) {
            self.advance()?;
            return Err(Box::new(ParserError::MissingToken {
                missing: ";".to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: Some(format!("let statements require a semicolon(;) at the end")),
            }));
        }
        let end_span = self.curr_token.as_ref().unwrap().span;
        self.advance()?;
        Ok(Statement::Let {
            span: Span {
                start_byte_pos: start_span.start_byte_pos,
                end_byte_pos: end_span.end_byte_pos,
                line_num: start_span.line_num,
                col_num: start_span.col_num,
            },
            identifier: identifier,
            value: expr,
        })
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement<'a>, Box<dyn Diagnostic>> {
        if !self.curr_token_is(TokenType::Return) {
            self.advance()?;
            return Err(Box::new(ParserError::UnexpectedToken {
                expected: "return".to_string(),
                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: Some(format!("return statements begins with \"return\" keyword")),
            }));
        }
        let start_span = self.curr_token.as_ref().unwrap().span;
        if self.peek_token_is(TokenType::Semicolon) {
            self.advance()?;
            let end_span = self.curr_token.as_ref().unwrap().span;
            self.advance()?;
            return Ok(Statement::Return {
                span: Span {
                    start_byte_pos: start_span.start_byte_pos,
                    end_byte_pos: end_span.end_byte_pos,
                    line_num: start_span.line_num,
                    col_num: start_span.col_num,
                },
                value: None,
            });
        }
        self.advance()?;
        let expr = self.parse_expression()?;
        if self.curr_token_is(TokenType::Semicolon) {
            self.advance()?;
            return Err(Box::new(ParserError::UnexpectedToken {
                expected: "return".to_string(),
                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: Some(format!("return statements begins with \"return\" keyword")),
            }));
        }

        let end_span = self.curr_token.as_ref().unwrap().span;
        self.advance()?;
        Ok(Statement::Return {
            span: Span {
                start_byte_pos: start_span.start_byte_pos,
                end_byte_pos: end_span.end_byte_pos,
                line_num: start_span.line_num,
                col_num: start_span.col_num,
            },
            value: Some(expr),
        })
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
        let start_span = self.curr_token.as_ref().unwrap().span;
        let expr = self.parse_expression()?;
        let end_span = self.curr_token.as_ref().unwrap().span;
        Ok(Statement::Expression {
            span: Span {
                start_byte_pos: start_span.start_byte_pos,
                end_byte_pos: end_span.end_byte_pos,
                line_num: start_span.line_num,
                col_num: start_span.col_num,
            },
            expr: expr,
        })
    }

    /*
     * fn function_definition(param_1, param_2, param_3) {
     *    // code
     * }
     * */
    fn parse_fn_definition(&mut self) -> Result<ast::Statement<'a>, Box<dyn Diagnostic>> {
        if !self.curr_token_is(TokenType::Function) {
            return Err(Box::new(ParserError::UnexpectedToken {
                expected: "fn".to_string(),
                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: Some(format!("function definitions starts with \"fn\" keyword")),
            }));
        }
        let start_span = self.curr_token.as_ref().unwrap().span;

        self.advance()?;
        if !self.curr_token_is(TokenType::Identifier) {
            return Err(Box::new(ParserError::UnexpectedToken {
                expected: "identifier".to_string(),
                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: Some("function definitions require an identifier. e.g. fn <func_identifier> (<params_list>) {<code>}".to_string()),
            }));
        }
        let identifier = self.curr_token.as_ref().unwrap().lexeme.to_string();

        self.advance()?;
        if !self.curr_token_is(TokenType::LParen) {
            return Err(Box::new(ParserError::UnexpectedToken {
                expected: "(".to_string(),
                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: None,
            }));
        }

        self.advance()?;
        let params_list = self.read_function_params()?;

        if !self.curr_token_is(TokenType::RParen) {
            return Err(Box::new(ParserError::MissingToken {
                missing: ")".to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: Some("unclosed parameter list. close the list by adding a ).".to_string()),
            }));
        }

        self.advance()?;
        if !self.curr_token_is(TokenType::LBrace) {
            return Err(Box::new(ParserError::UnexpectedToken {
                expected: "{".to_string(),
                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: Some("function blocks begin with {".to_string()),
            }));
        }

        self.advance()?;
        let fn_block = self.parse_expression()?;

        self.advance()?;
        if !self.curr_token_is(TokenType::RBrace) {
            return Err(Box::new(ParserError::UnexpectedToken {
                expected: "}".to_string(),
                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: Some("unclosed function block. close the block with }".to_string()),
            }));
        }

        let end_span = self.curr_token.as_ref().unwrap().span;
        self.advance()?;
        Ok(Statement::FunctionDef {
            span: Span {
                start_byte_pos: start_span.start_byte_pos,
                end_byte_pos: end_span.end_byte_pos,
                line_num: start_span.line_num,
                col_num: start_span.col_num,
            },
            identifier: identifier,
            params: params_list,
            body: fn_block,
        })
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

    fn sync(&mut self, sync_tokens: &[TokenType]) -> Result<(), Box<dyn Diagnostic>> {
        while self
            .curr_token
            .as_ref()
            .map(|t| !sync_tokens.contains(&t.t_type) && t.t_type != TokenType::Eof)
            .unwrap_or(false)
        {
            self.advance()?;
        }
        Ok(())
    }

    // fn function_def (param1, param2, param3)
    //                  ^                    ^
    // read paramter alists
    fn read_function_params(&mut self) -> Result<Vec<String>, Box<dyn Diagnostic>> {
        let mut param_list: Vec<String> = vec![];
        if self.curr_token_is(TokenType::RParen) {
            return Ok(param_list);
        }

        if !self.curr_token_is(TokenType::Identifier) {
            return Err(Box::new(ParserError::UnexpectedToken {
                expected: "identifier".to_string(),
                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: None,
            }));
        }
        param_list.push(self.curr_token.as_ref().unwrap().lexeme.to_string());
        self.advance()?;

        while self.curr_token_is(TokenType::Comma) {
            self.advance()?;
            if !self.curr_token_is(TokenType::Identifier) {
                return Err(Box::new(ParserError::UnexpectedToken {
                    expected: "identifier".to_string(),
                    found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                    span: self.curr_token.as_ref().unwrap().span,
                    hint: None,
                }));
            }
            param_list.push(self.curr_token.as_ref().unwrap().lexeme.to_string());
            self.advance()?;
        }
        Ok(param_list)
    }

    // call(arg1, arg2, arg3)
    //      ^              ^
    // read function argument list
    fn read_function_args(&mut self) -> Result<Vec<Expression<'a>>, Box<dyn Diagnostic>> {
        let mut args_list: Vec<Expression<'a>> = vec![];
        if self.curr_token_is(TokenType::RParen) {
            return Ok(args_list);
        }

        args_list.push(self.parse_expression()?);

        while self.curr_token_is(TokenType::Comma) {
            self.advance()?;
            args_list.push(self.parse_expression()?);
        }
        Ok(args_list)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Lexer,
        parser::{Parser, ast::Statement},
    };

    #[test]
    fn test_parse_let_statement() {
        let source = "let x := 10;
        let y := 20; 
        let foobar := 8383;";

        let mut lexer = Lexer::new(source);

        let parser_res = Parser::new(&mut lexer);
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

        let mut lexer = Lexer::new(source);

        let parser_res = Parser::new(&mut lexer);
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
