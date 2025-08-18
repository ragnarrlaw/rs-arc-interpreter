// TODO: Add a type system to the language -> Number, String, Char, Boolean, Unit, Fn, Undefined, Objects, Custom types and so on.

use std::collections::HashMap;

use crate::lexer::span::Span;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Number,
    String,
    Char,
    Boolean,
    Unit, // corresponds to the unit type of null set {} or ()
    Fn {
        param_types: Vec<Self>,
        return_type: Box<Type>,
    },
    Undefined, // for inferred or untyped parameters
}

#[derive(Debug)]
pub struct Symbol {
    typ: Type,
    span: Span,
    name: String,
}

type Scope = HashMap<String, Symbol>;

#[derive(Debug)]
pub struct SymbolTable {}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {}
    }
}
