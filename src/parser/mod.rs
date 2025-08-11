use crate::{
    error::{diagnostic::Diagnostic, parser_error::ParserError},
    lexer::{
        Lexer,
        span::Span,
        token::{Token, TokenType},
    },
    parser::{
        ast::{Expression, Operator, Program, Statement},
        precedence::Precedence,
    },
};

pub mod ast;
pub mod precedence;

type ParsePrefixFn<'a> = fn(&mut Parser<'a>) -> Result<Expression<'a>, Box<dyn Diagnostic>>;
type ParseInfixFn<'a> = fn(
    parser: &mut Parser<'a>,
    lhs: ast::Expression<'a>,
) -> Result<Expression<'a>, Box<dyn Diagnostic>>;
type ParsePostfixFn<'a> = fn(
    parser: &mut Parser<'a>,
    lhs: ast::Expression<'a>,
) -> Result<Expression<'a>, Box<dyn Diagnostic>>;

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

        let expr = self.parse_expression(Precedence::Lowest as i8)?;

        self.advance()?;
        if !self.curr_token_is(TokenType::Semicolon) {
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
        let expr = self.parse_expression(Precedence::Lowest as i8)?;
        self.advance()?;
        if !self.curr_token_is(TokenType::Semicolon) {
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

    fn parse_expression_statement(&mut self) -> Result<ast::Statement<'a>, Box<dyn Diagnostic>> {
        let start_span = self.curr_token.as_ref().unwrap().span;
        let expr = self.parse_expression(Precedence::Lowest as i8)?;
        let end_span = self.curr_token.as_ref().unwrap().span;
        if self.curr_token_is(TokenType::Semicolon) {
            self.advance()?;
        }
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
        let fn_block = self.parse_expression(Precedence::Lowest as i8)?;

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

    fn parse_expression(
        &mut self,
        min_precedence: i8,
    ) -> Result<ast::Expression<'a>, Box<dyn Diagnostic>> {
        let prefix_fn = Self::get_prefix_parse_fn(self.curr_token.as_ref().unwrap().t_type);
        if let Some(prefix_fn) = prefix_fn {
            let mut left = prefix_fn(self)?;
            loop {
                if self.peek_token_is(TokenType::Semicolon)
                    || self.peek_token_is(TokenType::RBrace)
                    || self.peek_token_is(TokenType::Eof)
                {
                    break;
                }

                let mut binding_power = Precedence::postfix_operator_binding_power(
                    self.peek_token.as_ref().unwrap().t_type,
                );

                if binding_power.0 > -1 {
                    if binding_power.0 < min_precedence {
                        break;
                    }
                    self.advance()?;

                    let postfix_fn = match Self::get_postfix_parse_fn(
                        self.curr_token.as_ref().unwrap().t_type,
                    ) {
                        Some(func) => func,
                        None => {
                            return Err(Box::new(ParserError::UnexpectedToken {
                                expected: "postfix operator".to_string(),
                                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                                span: self.curr_token.as_ref().unwrap().span,
                                hint: None,
                            }));
                        }
                    };

                    left = postfix_fn(self, left)?;
                    continue;
                }

                binding_power = Precedence::infix_operator_binding_power(
                    self.peek_token.as_ref().unwrap().t_type,
                );

                if binding_power.0 > -1 {
                    if binding_power.0 < min_precedence {
                        break;
                    }

                    self.advance()?;

                    let infix_fn =
                        match Self::get_infix_parse_fn(self.curr_token.as_ref().unwrap().t_type) {
                            Some(func) => func,
                            None => {
                                return Err(Box::new(ParserError::UnexpectedToken {
                                    expected: "infix operator".to_string(),
                                    found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                                    span: self.curr_token.as_ref().unwrap().span,
                                    hint: None,
                                }));
                            }
                        };
                    left = infix_fn(self, left)?;
                    continue;
                }
                break;
            }
            Ok(left)
        } else {
            Err(Box::new(ParserError::UnexpectedToken {
                expected: "expression".to_string(),
                found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                span: self.curr_token.as_ref().unwrap().span,
                hint: None,
            }))
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<ast::Expression<'a>, Box<dyn Diagnostic>> {
        let start_span = self.curr_token.as_ref().unwrap().span;

        let operator = match Operator::new(
            self.curr_token.as_ref().unwrap().t_type,
            self.curr_token.as_ref().unwrap().span,
        ) {
            Some(op) => op,
            None => {
                return Err(Box::new(ParserError::UnexpectedToken {
                    expected: "expression".to_string(),
                    found: self.curr_token.as_ref().unwrap().lexeme.to_string(),
                    span: self.curr_token.as_ref().unwrap().span,
                    hint: Some("<prefix operator><expression>".to_string()),
                }));
            }
        };

        self.advance()?;
        let rhs = self.parse_expression(Precedence::Prefix as i8)?;
        let end_span = self.curr_token.as_ref().unwrap().span;
        self.advance()?;

        Ok(Expression::PrefixExpression {
            span: Span {
                start_byte_pos: start_span.start_byte_pos,
                end_byte_pos: end_span.end_byte_pos,
                line_num: start_span.line_num,
                col_num: start_span.col_num,
            },
            op: operator,
            right_expr: Box::new(rhs),
        })
    }

    fn parse_infix_expression(
        &mut self,
        lhs: ast::Expression<'a>,
    ) -> Result<ast::Expression<'a>, Box<dyn Diagnostic>> {
        let start_span = lhs.span();
        let operator_token = self.curr_token.as_ref().unwrap();
        let operator_binding_power =
            Precedence::infix_operator_binding_power(operator_token.t_type);
        let operator = match Operator::new(operator_token.t_type, operator_token.span) {
            Some(op) => op,
            None => {
                return Err(Box::new(ParserError::UnexpectedToken {
                    expected: "binary operator".to_string(),
                    found: operator_token.lexeme.to_string(),
                    span: operator_token.span,
                    hint: None,
                }));
            }
        };

        self.advance()?;
        let rhs = self.parse_expression(operator_binding_power.1)?;
        let end_span = rhs.span();
        Ok(Expression::InfixExpression {
            span: Span {
                start_byte_pos: start_span.start_byte_pos,
                end_byte_pos: end_span.end_byte_pos,
                line_num: start_span.line_num,
                col_num: start_span.col_num,
            },
            left_expr: Box::new(lhs),
            op: operator,
            right_expr: Box::new(rhs),
        })
    }

    fn parse_postfix_expression(
        &mut self,
        lhs: ast::Expression<'a>,
    ) -> Result<ast::Expression<'a>, Box<dyn Diagnostic>> {
        let start_span = lhs.span();

        let operator_token = self.curr_token.as_ref().unwrap();
        let operator = match Operator::new(operator_token.t_type, operator_token.span) {
            Some(op) => op,
            None => {
                return Err(Box::new(ParserError::UnexpectedToken {
                    expected: "unary postfix operator".to_string(),
                    found: operator_token.lexeme.to_string(),
                    span: operator_token.span,
                    hint: None,
                }));
            }
        };

        let end_span = self.curr_token.as_ref().unwrap().span;

        Ok(Expression::PostfixExpression {
            span: Span {
                start_byte_pos: start_span.start_byte_pos,
                end_byte_pos: end_span.end_byte_pos,
                line_num: start_span.line_num,
                col_num: start_span.col_num,
            },
            left_expr: Box::new(lhs),
            op: operator,
        })
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

        args_list.push(self.parse_expression(Precedence::Lowest as i8)?);

        while self.curr_token_is(TokenType::Comma) {
            self.advance()?;
            args_list.push(self.parse_expression(Precedence::Lowest as i8)?);
        }
        Ok(args_list)
    }

    fn get_prefix_parse_fn(token_type: TokenType) -> Option<ParsePrefixFn<'a>> {
        match token_type {
            TokenType::Identifier => Some(Self::parse_identifier),
            TokenType::Number => Some(Self::parse_number_literal),
            TokenType::String => Some(Self::parse_string_literal),
            TokenType::Char => Some(Self::parse_char_literal),
            TokenType::True | TokenType::False => Some(Self::parse_boolean_literal),
            TokenType::Not
            | TokenType::Inc
            | TokenType::Dec
            | TokenType::Minus
            | TokenType::Plus => Some(Self::parse_prefix_expression),
            _ => None,
        }
    }

    fn get_infix_parse_fn(token_type: TokenType) -> Option<ParseInfixFn<'a>> {
        match token_type {
            TokenType::Plus
            | TokenType::Slash
            | TokenType::Minus
            | TokenType::Asterix
            | TokenType::Mod
            | TokenType::Dot
            | TokenType::Gt
            | TokenType::Lt
            | TokenType::Or
            | TokenType::And
            | TokenType::EqEq
            | TokenType::LtEq
            | TokenType::GtEq => Some(Self::parse_infix_expression),
            _ => None,
        }
    }

    fn get_postfix_parse_fn(token_type: TokenType) -> Option<ParsePostfixFn<'a>> {
        match token_type {
            TokenType::Inc | TokenType::Dec => Some(Self::parse_postfix_expression),
            _ => None,
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression<'a>, Box<dyn Diagnostic>> {
        return Ok(Expression::Identifier {
            identifier: self.curr_token.as_ref().unwrap().lexeme,
            span: self.curr_token.as_ref().unwrap().span,
        });
    }

    fn parse_number_literal(&mut self) -> Result<Expression<'a>, Box<dyn Diagnostic>> {
        let token = self.curr_token.as_ref().unwrap();
        let lexeme = token.lexeme.replace("_", "");
        match lexeme.parse::<f64>() {
            Ok(val) => Ok(Expression::Number {
                span: token.span,
                val,
            }),
            Err(_) => Err(Box::new(ParserError::InvalidNumberValue(
                lexeme, token.span,
            ))),
        }
    }

    fn parse_boolean_literal(&mut self) -> Result<Expression<'a>, Box<dyn Diagnostic>> {
        let token = self.curr_token.as_ref().unwrap();
        match token.t_type {
            TokenType::True => Ok(Expression::Boolean {
                span: token.span,
                val: true,
            }),
            TokenType::False => Ok(Expression::Boolean {
                span: token.span,
                val: false,
            }),
            _ => Err(Box::new(ParserError::UnexpectedToken {
                expected: "true or false".to_string(),
                found: token.lexeme.to_string(),
                span: token.span,
                hint: None,
            })),
        }
    }

    fn parse_string_literal(&mut self) -> Result<Expression<'a>, Box<dyn Diagnostic>> {
        let token = self.curr_token.as_ref().unwrap();
        let processed = Self::process_escape_sequences(token.lexeme, token.span)?;
        Ok(Expression::String {
            span: token.span,
            val: processed,
        })
    }

    fn parse_char_literal(&mut self) -> Result<Expression<'a>, Box<dyn Diagnostic>> {
        let token = self.curr_token.as_ref().unwrap();
        let processed = Self::process_escape_sequences(token.lexeme, token.span)?;
        if processed.chars().count() != 1 {
            return Err(Box::new(ParserError::InvalidCharValue(
                token.lexeme.to_string(),
                token.span,
            )));
        }
        let val = processed.chars().next().unwrap();
        Ok(Expression::Char {
            span: token.span,
            val,
        })
    }

    // helper: processes escape sequences in strings and characters (e.g., "\n", "\uXXXX").
    fn process_escape_sequences(lexeme: &str, span: Span) -> Result<String, Box<dyn Diagnostic>> {
        let mut result = String::new();
        let mut chars = lexeme.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == '\\' {
                if let Some(next_ch) = chars.next() {
                    match next_ch {
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        '\'' => result.push('\''),
                        'u' => {
                            let mut code = String::new();
                            for _ in 0..4 {
                                if let Some(c) = chars.next() {
                                    // is hex digit
                                    if c.is_digit(16) {
                                        code.push(c);
                                    } else {
                                        return Err(Box::new(ParserError::InvalidEscapeSequence(
                                            format!("\\u{}", code),
                                            span,
                                        )));
                                    }
                                } else {
                                    return Err(Box::new(ParserError::InvalidEscapeSequence(
                                        "\\u".to_string(),
                                        span,
                                    )));
                                }
                            }
                            if let Ok(code_point) = u32::from_str_radix(&code, 16) {
                                if let Some(unicode_char) = char::from_u32(code_point) {
                                    result.push(unicode_char);
                                } else {
                                    return Err(Box::new(ParserError::InvalidEscapeSequence(
                                        format!("\\u{}", code),
                                        span,
                                    )));
                                }
                            } else {
                                return Err(Box::new(ParserError::InvalidEscapeSequence(
                                    format!("\\u{}", code),
                                    span,
                                )));
                            }
                        }
                        _ => {
                            return Err(Box::new(ParserError::InvalidEscapeSequence(
                                format!("\\{}", next_ch),
                                span,
                            )));
                        }
                    }
                } else {
                    return Err(Box::new(ParserError::InvalidEscapeSequence(
                        "\\".to_string(),
                        span,
                    )));
                }
            } else {
                result.push(ch);
            }
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        error::report::Report,
        lexer::Lexer,
        line_map::LineMap,
        parser::{
            Parser,
            ast::{Expression, Operator, Statement},
        },
    };

    #[test]
    fn test_parse_let_statement() {
        let source = "let x := 10;
        let y := 20; 
        let foobar := 8383;
        let new_foobar := true;
        let n := 'n';
        let a := \"Hello, World\n\";
        let num := 1_000_000.00;
        let scientific := 1_123.45e-6;";

        let mut lexer = Lexer::new(source);

        let parser_res = Parser::new(&mut lexer);
        match parser_res {
            Ok(mut parser) => {
                match parser.parse_program() {
                    Ok(program) => {
                        assert_eq!(program.statements.len(), 8);

                        match &program.statements[0] {
                            Statement::Let {
                                identifier, value, ..
                            } => {
                                assert_eq!(identifier, "x");
                                match value {
                                    Expression::Number { val, .. } => assert_eq!(*val, 10.00),
                                    _ => panic!("invalid expression"),
                                }
                            }
                            _ => panic!("invalid statement"),
                        }
                        match &program.statements[1] {
                            Statement::Let {
                                identifier, value, ..
                            } => {
                                assert_eq!(identifier, "y");
                                match value {
                                    Expression::Number { val, .. } => assert_eq!(*val, 20.00),
                                    _ => panic!("invalid expression"),
                                }
                            }
                            _ => panic!("invalid statement"),
                        }
                        match &program.statements[2] {
                            Statement::Let {
                                identifier, value, ..
                            } => {
                                assert_eq!(identifier, "foobar");
                                match value {
                                    Expression::Number { val, .. } => assert_eq!(*val, 8383.00),
                                    _ => panic!("invalid expression"),
                                }
                            }
                            _ => panic!("invalid statement"),
                        }
                        match &program.statements[3] {
                            Statement::Let {
                                identifier, value, ..
                            } => {
                                assert_eq!(identifier, "new_foobar");
                                match value {
                                    Expression::Boolean { val, .. } => assert_eq!(*val, true),
                                    _ => panic!("invalid expression"),
                                }
                            }
                            _ => panic!("invalid statement"),
                        }
                        match &program.statements[4] {
                            Statement::Let {
                                identifier, value, ..
                            } => {
                                assert_eq!(identifier, "n");
                                match value {
                                    Expression::Char { val, .. } => assert_eq!(*val, 'n'),
                                    _ => panic!("invalid expression"),
                                }
                            }
                            _ => panic!("invalid statement"),
                        }
                        match &program.statements[5] {
                            Statement::Let {
                                identifier, value, ..
                            } => {
                                assert_eq!(identifier, "a");
                                match value {
                                    Expression::String { val, .. } => {
                                        assert_eq!(val, "Hello, World\n")
                                    }
                                    _ => panic!("invalid expression"),
                                }
                            }
                            _ => panic!("invalid statement"),
                        }
                        match &program.statements[6] {
                            Statement::Let {
                                identifier, value, ..
                            } => {
                                assert_eq!(identifier, "num");
                                match value {
                                    Expression::Number { val, .. } => {
                                        assert_eq!(*val, 1_000_000.00)
                                    }
                                    _ => panic!("invalid expression"),
                                }
                            }
                            _ => panic!("invalid statement"),
                        }
                        match &program.statements[7] {
                            Statement::Let {
                                identifier, value, ..
                            } => {
                                assert_eq!(identifier, "scientific");
                                match value {
                                    Expression::Number { val, .. } => assert_eq!(*val, 1123.45e-6),
                                    _ => panic!("invalid expression"),
                                }
                            }
                            _ => panic!("invalid statement"),
                        }
                    }
                    Err(err) => {
                        let line_map = LineMap::new(source);
                        let reporter = Report::new(&source, line_map, &*err);
                        panic!(
                            "parser failed to identify the let statements - {}",
                            reporter
                        )
                    }
                };
            }
            Err(err) => {
                let line_map = LineMap::new(source);
                let reporter = Report::new(&source, line_map, &*err);
                panic!(
                    "parser failed to identify the let statements - {}",
                    reporter
                )
            }
        };
    }

    #[test]
    fn test_parse_return_statements() {
        let source = "return 10;
        return x;";

        let mut lexer = Lexer::new(source);

        let parser_res = Parser::new(&mut lexer);
        match parser_res {
            Ok(mut parser) => {
                match parser.parse_program() {
                    Ok(program) => {
                        assert_eq!(program.statements.len(), 2);
                    }
                    Err(err) => {
                        let line_map = LineMap::new(source);
                        let reporter = Report::new(&source, line_map, &*err);
                        panic!(
                            "parser failed to identify the return statements - {}",
                            reporter
                        )
                    }
                };
            }
            Err(err) => {
                let line_map = LineMap::new(source);
                let reporter = Report::new(&source, line_map, &*err);
                panic!(
                    "parser failed to identify the let statements - {}",
                    reporter
                )
            }
        };
    }

    #[test]
    fn test_parse_prefix_expression() {
        let source = "!false;
        -100;
        +10;
        --10;
        ++10;
        !true";

        let mut lexer = Lexer::new(source);

        let parser_res = Parser::new(&mut lexer);
        match parser_res {
            Ok(mut parser) => {
                match parser.parse_program() {
                    Ok(program) => {
                        assert_eq!(program.statements.len(), 6);
                        match &program.statements[0] {
                            Statement::Expression { span: _, expr } => match expr {
                                Expression::PrefixExpression {
                                    span: _,
                                    op: Operator::Negation { span: _ },
                                    right_expr,
                                } => match **right_expr {
                                    Expression::Boolean { span: _, val } => assert_eq!(val, false),
                                    _ => panic!(
                                        "parser failed to identify the expression as boolean"
                                    ),
                                },
                                _ => panic!(
                                    "parser failed to identify the expression as a prefix expression"
                                ),
                            },
                            _ => panic!(
                                "parser failed to identify the statement as an expression statements"
                            ),
                        }
                        match &program.statements[5] {
                            Statement::Expression { span: _, expr } => match expr {
                                Expression::PrefixExpression {
                                    span: _,
                                    op: Operator::Negation { span: _ },
                                    right_expr,
                                } => match **right_expr {
                                    Expression::Boolean { span: _, val } => assert_eq!(val, true),
                                    _ => panic!(
                                        "parser failed to identify the expression as boolean"
                                    ),
                                },
                                _ => panic!(
                                    "parser failed to identify the expression as a prefix expression"
                                ),
                            },
                            _ => panic!(
                                "parser failed to identify the statement as an expression statements"
                            ),
                        }
                    }
                    Err(err) => {
                        let line_map = LineMap::new(source);
                        let reporter = Report::new(&source, line_map, &*err);
                        panic!("parser failed to parse program - {}", reporter)
                    }
                };
            }
            Err(err) => {
                let line_map = LineMap::new(source);
                let reporter = Report::new(&source, line_map, &*err);
                panic!("parser failed to initialize - {}", reporter)
            }
        };
    }

    #[test]
    #[test]
    fn test_infix_expression() {
        let source = "x + y;";
        let mut lexer = Lexer::new(source);
        let mut parser = Parser::new(&mut lexer).unwrap();
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression { expr, .. } => match expr {
                Expression::InfixExpression {
                    left_expr,
                    op,
                    right_expr,
                    span,
                } => {
                    assert!(matches!(
                        left_expr.as_ref(),
                        Expression::Identifier {
                            identifier: "x",
                            ..
                        }
                    ));
                    assert!(matches!(op, Operator::Plus { .. }));
                    assert!(matches!(
                        right_expr.as_ref(),
                        Expression::Identifier {
                            identifier: "y",
                            ..
                        }
                    ));
                    assert_eq!(span.start_byte_pos, 0);
                    assert_eq!(span.end_byte_pos, 5);
                }
                _ => panic!("Expected infix expression"),
            },
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_postfix_expression() {
        let source = "x++;";
        let mut lexer = Lexer::new(source);
        let mut parser = Parser::new(&mut lexer).unwrap();
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression { expr, .. } => match expr {
                Expression::PostfixExpression {
                    left_expr,
                    op,
                    span,
                } => {
                    assert!(matches!(
                        left_expr.as_ref(),
                        Expression::Identifier {
                            identifier: "x",
                            ..
                        }
                    ));
                    assert!(matches!(op, Operator::Increment { .. }));
                    assert_eq!(span.start_byte_pos, 0);
                    assert_eq!(span.end_byte_pos, 4);
                }
                _ => panic!("Expected postfix expression"),
            },
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_invalid_infix() {
        let source = "x + ;";
        let mut lexer = Lexer::new(source);
        let mut parser = Parser::new(&mut lexer).unwrap();
        let result = parser.parse_program();
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message().contains("expected expression"));
    }
}
