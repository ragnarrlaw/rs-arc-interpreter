use crate::lexer::span::Span;

#[derive(Debug)]
pub struct Construct {
    pub span: Span,
    pub msg: Option<String>,
}

impl Construct {
    pub fn new(span: Span, msg: Option<String>) -> Self {
        Self {
            span: span,
            msg: msg,
        }
    }
}

#[derive(Debug)]
pub struct Context(pub Vec<Construct>);

impl Context {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push(&mut self, span: Span, msg: Option<String>) -> () {
        self.0.push(Construct::new(span, msg))
    }

    pub fn pop(&mut self) -> Option<Construct> {
        self.0.pop()
    }
}
