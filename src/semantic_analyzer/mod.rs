use std::collections::HashMap;

use crate::{
    error::{
        context::Context,
        diagnostic::Diagnostic,
        semantic_error::{SemanticError, SemanticErrorType::RedeclarationOfSameSymbol},
    },
    lexer::span::Span,
    parser::ast::{Expression, Program, Statement},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Number,
    String,
    Char,
    Boolean,
    Unit, // corresponds to the unit type of null set {} or ()
    Function {
        param_types: Vec<Self>,
        return_type: Box<Type>,
    },
    Unknown, // for inferred or untyped parameters
}

#[derive(Debug)]
pub struct Symbol {
    type_of: Type,
    span: Span,
}

type Scope = HashMap<String, Symbol>;

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()],
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn insert(
        &mut self,
        name: String,
        type_of: Type,
        span: Span,
    ) -> Result<(), Box<dyn Diagnostic>> {
        let current_scope = self.scopes.last_mut().unwrap();
        if let Some(symbol) = current_scope.get(&name) {
            let mut ctx = Context::new();
            ctx.push(
                symbol.span,
                format!("variable with the {} is previously declared here", name).into(),
            );
            Err(Box::new(SemanticError::new(
                Some(ctx),
                RedeclarationOfSameSymbol(name),
                span,
                None,
            )))
        } else {
            current_scope.insert(
                name,
                Symbol {
                    type_of: type_of,
                    span: span,
                },
            );
            Ok(())
        }
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }
}

pub struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    errors: Vec<Box<dyn Diagnostic>>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut analyzer = SemanticAnalyzer {
            symbol_table: SymbolTable::new(),
            errors: vec![],
        };
        analyzer.add_builtins();
        analyzer
    }

    fn add_builtins(&mut self) {
        self.symbol_table
            .insert(
                "print".to_string(),
                Type::Function {
                    param_types: vec![Type::Unknown],
                    return_type: Box::new(Type::Unit),
                },
                Span::default(),
            )
            .unwrap();
    }

    fn analyze_program(&mut self, program: &Program) {
        for stmt in &program.statements {
            if let Err(err) = self.analyze_statement(stmt) {
                self.errors.push(err);
            }
        }
    }

    fn analyze_statement(&mut self, stmt: &Statement) -> Result<Type, Box<dyn Diagnostic>> {
        todo!()
    }

    fn analyze_expression(&mut self, expr: &Expression) -> Result<Type, Box<dyn Diagnostic>> {
        todo!()
    }
}
