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
