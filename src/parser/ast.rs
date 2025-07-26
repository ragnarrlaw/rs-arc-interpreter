use std::sync::Arc;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Let {
        identifier: String,
        value: Expression,
    },
    Return {
        value: Option<Expression>,
    },
    Expression(Expression),
    FunctionDef {
        identifier: String,
        params: Vec<String>,
        body: Expression,
    },
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Sub,
    Divide,
    Multiply,
    Dot,
}

#[derive(Debug)]
pub enum Expression {
    Number(f64),
    String(String),
    Boolean(bool),
    Char(char),
    InfixExpression(Box<Expression>, Operator, Box<Expression>),
    Block {
        statements: Vec<Statement>,
        return_expr: Option<Box<Expression>>,
    },
    If {
        condition: Box<Expression>,
        consequence: Box<Expression>,
        alternative: Option<Box<Expression>>,
    },
    Function {
        params: Vec<String>,
        body: Box<Expression>,
    },
}
