use std::fmt::Display;
use std::vec::Vec;

use crate::lexer::span::Span;
use crate::lexer::token::TokenType;

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

impl<'a> Statement<'a> {
    pub fn span(&self) -> Span {
        match self {
            Self::Let { span, .. } => *span,
            Self::Return { span, .. } => *span,
            Self::Expression { span, .. } => *span,
            Self::FunctionDef { span, .. } => *span,
        }
    }
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
            Self::FunctionDef {
                span: _,
                identifier,
                params,
                body,
            } => write!(
                f,
                "<fn def>:{} <params>:{} <body>: {}",
                identifier,
                params.join(", "),
                body
            ),
        }
    }
}

#[derive(Debug)]
pub enum Operator {
    Plus { span: Span },
    Sub { span: Span },
    Divide { span: Span },
    Multiply { span: Span },
    Modulo { span: Span },
    Access { span: Span },
    Negation { span: Span },
    Increment { span: Span },
    Decrement { span: Span },
    Equals { span: Span },
    NotEquals { span: Span },
    GreaterThan { span: Span },
    GreaterThanEquals { span: Span },
    LessThan { span: Span },
    LessThanEquals { span: Span },
    And { span: Span },
    Or { span: Span },
}

impl Operator {
    pub fn new(token_type: TokenType, span: Span) -> Option<Operator> {
        match token_type {
            TokenType::Plus => Some(Self::Plus { span: span }),
            TokenType::Minus => Some(Self::Sub { span: span }),
            TokenType::Slash => Some(Self::Divide { span: span }),
            TokenType::Asterix => Some(Self::Multiply { span: span }),
            TokenType::Dot => Some(Self::Access { span: span }),
            TokenType::Mod => Some(Self::Modulo { span: span }),
            TokenType::Not => Some(Self::Negation { span: span }),
            TokenType::Inc => Some(Self::Increment { span: span }),
            TokenType::Dec => Some(Self::Decrement { span: span }),
            TokenType::EqEq => Some(Self::Equals { span: span }),
            TokenType::NotEq => Some(Self::NotEquals { span: span }),
            TokenType::Gt => Some(Self::GreaterThan { span: span }),
            TokenType::GtEq => Some(Self::GreaterThanEquals { span: span }),
            TokenType::Lt => Some(Self::LessThan { span: span }),
            TokenType::LtEq => Some(Self::LessThanEquals { span: span }),
            TokenType::And => Some(Self::And { span: span }),
            TokenType::Or => Some(Self::Or { span: span }),
            _ => None,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus { span: _ } => write!(f, "+"),
            Self::Sub { span: _ } => write!(f, "-"),
            Self::Divide { span: _ } => write!(f, "/"),
            Self::Multiply { span: _ } => write!(f, "*"),
            Self::Modulo { span: _ } => write!(f, "%"),
            Self::Access { span: _ } => write!(f, "."),
            Self::Negation { span: _ } => write!(f, "!"),
            Self::Increment { span: _ } => write!(f, "++"),
            Self::Decrement { span: _ } => write!(f, "--"),
            Self::Equals { span: _ } => write!(f, "=="),
            Self::NotEquals { span: _ } => write!(f, "!="),
            Self::LessThan { span: _ } => write!(f, "<"),
            Self::LessThanEquals { span: _ } => write!(f, "<="),
            Self::GreaterThan { span: _ } => write!(f, ">"),
            Self::GreaterThanEquals { span: _ } => write!(f, ">="),
            Self::And { span: _ } => write!(f, "&&"),
            Self::Or { span: _ } => write!(f, "||"),
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
        right_expr: Box<Expression<'a>>,
    },
    InfixExpression {
        span: Span,
        left_expr: Box<Expression<'a>>,
        op: Operator,
        right_expr: Box<Expression<'a>>,
    },
    PostfixExpression {
        span: Span,
        left_expr: Box<Expression<'a>>,
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
            Self::Number { span: _, val } => write!(f, "<Number>: {}", val),
            Self::String { span: _, val } => write!(f, "<String>: \"{}\"", val),
            Self::Boolean { span: _, val } => write!(f, "<Boolean>: {}", val),
            Self::Char { span: _, val } => write!(f, "<Char>: '{}'", val),
            Self::Identifier {
                identifier,
                span: _,
            } => write!(f, "<Identifier>: {}", identifier),
            Self::PrefixExpression {
                span: _,
                op,
                right_expr,
            } => write!(f, "({}{})", op, right_expr),
            Self::InfixExpression {
                span: _,
                left_expr,
                op,
                right_expr,
            } => write!(f, "({} {} {})", left_expr, op, right_expr),
            Self::PostfixExpression {
                span: _,
                left_expr,
                op,
            } => write!(f, "({}{})", left_expr, op),
            _ => todo!(),
        }
    }
}

impl<'a> Expression<'a> {
    pub fn span(&self) -> Span {
        match self {
            Self::Number { span, .. } => *span,
            Self::String { span, .. } => *span,
            Self::Boolean { span, .. } => *span,
            Self::Char { span, .. } => *span,
            Self::Identifier { span, .. } => *span,
            Self::PrefixExpression { span, .. } => *span,
            Self::InfixExpression { span, .. } => *span,
            Self::PostfixExpression { span, .. } => *span,
            Self::Block { span, .. } => *span,
            Self::If { span, .. } => *span,
            Self::Lambda { span, .. } => *span,
            Self::FnCall { span, .. } => *span,
        }
    }
}
