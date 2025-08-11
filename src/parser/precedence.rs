use std::i8;

use crate::lexer::token::{Token, TokenType};

pub type OperatorPrecedence = (i8, i8);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Precedence {
    Lowest = 0,     // Lowest precedence (e.g., commas)
    Assignment,     // :=, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=
    Conditional,    // ?: (ternary conditional)
    LogicalOr,      // ||
    LogicalAnd,     // &&
    BitwiseOr,      // |
    BitwiseXOr,     // ^
    BitwiseAnd,     // &
    Equality,       // ==, !=
    Relational,     // <, <=, >, >=
    Shift,          // <<, >>
    Additive,       // +, -
    Multiplicative, // *, /, %
    Prefix,         // -, !, ~, ++, --, sizeof, & (address-of), * (dereference), cast
    Postfix, // ++, --, () (function call), [] (array access), . (member access), -> (pointer member access)
    Highest, // Highest precedence (e.g., primary expressions like literals and parentheses)
}

impl Precedence {
    pub fn prefix_operator_binding_power(token_type: TokenType) -> OperatorPrecedence {
        match token_type {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Inc
            | TokenType::Dec
            | TokenType::Not => (Self::Prefix as i8, (Self::Prefix as i8) + 1),
            _ => (-1, -1),
        }
    }

    pub fn infix_operator_binding_power(token_type: TokenType) -> OperatorPrecedence {
        match token_type {
            TokenType::Plus | TokenType::Minus => {
                (Self::Additive as i8, (Self::Additive as i8) + 1)
            }
            TokenType::Mod | TokenType::Asterix | TokenType::Slash => {
                (Self::Multiplicative as i8, (Self::Multiplicative as i8) + 1)
            }
            TokenType::EqEq | TokenType::NotEq => {
                (Self::Equality as i8, (Self::Equality as i8) + 1)
            }
            TokenType::Lt | TokenType::LtEq | TokenType::Gt | TokenType::GtEq => {
                (Self::Relational as i8, (Self::Relational as i8) + 1)
            }
            TokenType::And => (Self::LogicalAnd as i8, (Self::LogicalAnd as i8) + 1),
            TokenType::Or => (Self::LogicalOr as i8, (Self::LogicalOr as i8) + 1),
            TokenType::Assign => (Self::Assignment as i8, (Self::Assignment as i8) - 1),
            TokenType::LParen => (Self::Postfix as i8, (Self::Postfix as i8) + 1),
            TokenType::Dot => (Self::Postfix as i8, (Self::Postfix as i8) + 1),
            _ => (-1, -1),
        }
    }

    pub fn postfix_operator_binding_power(token_type: TokenType) -> OperatorPrecedence {
        match token_type {
            TokenType::Inc | TokenType::Dec => (Self::Postfix as i8, (Self::Postfix as i8) + 1),
            _ => (-1, -1),
        }
    }
}
