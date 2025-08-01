use std::fmt::Display;
use std::vec::Vec;

use crate::lexer::span::Span;

#[derive(Debug)]
pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> Display for Program<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut tmp_f = String::new();
        for stmt in &self.statements {
            tmp_f.push_str(&format!("{}\n", stmt));
        }
        write!(f, "<Program>\n{}", tmp_f)
    }
}

#[derive(Debug)]
pub enum Statement<'a> {
    Let {
        span: Span, // span of the let -> "let ... := ...;"
        identifier: String,
        value: Expression<'a>,
    },
    Return {
        span: Span,
        value: Option<Expression<'a>>,
    },
    Expression {
        span: Span,
        expr: Expression<'a>,
    },
    FunctionDef {
        span: Span,
        identifier: String,
        params: Vec<String>,
        body: Expression<'a>,
    },
}

impl<'a> Display for Statement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let {
                span: _,
                identifier,
                value,
            } => write!(f, "<binding> {} := {};", identifier, value),
            Self::Return { span: _, value } => match value {
                Some(expr) => write!(f, "<return> {};", expr),
                None => write!(f, "<return>"),
            },
            Self::Expression { span: _, expr } => write!(f, "<expression> {}", expr),
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum Operator {
    Plus { span: Span },
    Sub { span: Span },
    Divide { span: Span },
    Multiply { span: Span },
    Dot { span: Span },
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus { span: _ } => write!(f, "+"),
            Self::Sub { span: _ } => write!(f, "-"),
            Self::Divide { span: _ } => write!(f, "/"),
            Self::Multiply { span: _ } => write!(f, "*"),
            Self::Dot { span: _ } => write!(f, "."),
        }
    }
}

#[derive(Debug)]
pub enum Expression<'a> {
    Number {
        span: Span,
        val: f64,
    },
    String {
        span: Span,
        val: String,
    },
    Boolean {
        span: Span,
        val: bool,
    },
    Char {
        span: Span,
        val: char,
    },
    Identifier {
        identifier: &'a str,
        span: Span,
    },
    PrefixExpression {
        span: Span,
        op: Operator,
        left_expr: Box<Expression<'a>>,
    },
    InfixExpression {
        span: Span,
        left_expr: Box<Expression<'a>>,
        op: Operator,
        right_expr: Box<Expression<'a>>,
    },
    PostfixExpression {
        span: Span,
        right_expr: Box<Expression<'a>>,
        op: Operator,
    },
    Block {
        span: Span,
        statements: Vec<Statement<'a>>,
        return_expr: Option<Box<Expression<'a>>>,
    },
    If {
        span: Span,
        condition: Box<Expression<'a>>,
        consequence: Box<Expression<'a>>,
        alternative: Option<Box<Expression<'a>>>,
    },
    Lambda {
        // anonymous functions
        span: Span,
        params: Vec<String>,
        body: Box<Expression<'a>>,
    },
    FnCall {
        // "fncall(...)" or "let lamda_call := fn () {}()"
        span: Span,
        func: Box<Expression<'a>>,
        args: Vec<Expression<'a>>,
    },
}

impl<'a> Display for Expression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number { span: _, val } => write!(f, "<Number> {}", val),
            Self::String { span: _, val } => write!(f, "<String> \"{}\"", val),
            Self::Boolean { span: _, val } => write!(f, "<Boolean> {}", val),
            Self::Char { span: _, val } => write!(f, "<Char> '{}'", val),
            _ => todo!(),
        }
    }
}
