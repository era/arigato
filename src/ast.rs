use std::mem;

use crate::lang::Token;
use itertools::PeekNth;
use itertools::peek_nth;

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
    tokens: PeekNth<std::vec::IntoIter<Token>>,
    curr: usize
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: peek_nth(tokens.into_iter()),
            curr: 0,
        }
    }

    fn advance(&mut self) -> Option<Token> {
        self.curr += 1;
        self.tokens.next()
    }

    fn expr(&mut self) -> Expr {
        todo!()
    }

    fn comparison(&mut self) -> Expr {
        todo!()
    }

    // a == b; a != b
    fn equality(&mut self) -> Expr {
        todo!()
    }
}