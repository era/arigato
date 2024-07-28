use crate::lang::Token;

pub enum Expr {
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>) 
}

///
/// Name        Operators   Associates
/// Equality    == !=       Left
/// Comparison  > >= <= <   Left
/// Term        - +         Left
/// Factor      / *         Left
/// Unary       ! -         Right
pub struct Parser {
    tokens: Vec<Token>,
    curr: usize
}