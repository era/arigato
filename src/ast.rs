use std::mem;

use crate::lang::Token;
use itertools::PeekNth;
use itertools::peek_nth;

pub enum ParserError {
    Generic
}

pub enum Expr {
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    EOF
}

pub type Result<I> = std::result::Result<I, ParserError>;

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
// Recursive Descent Parsing
impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: peek_nth(tokens.into_iter()),
            curr: 0,
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn advance(&mut self) -> Option<Token> {
        self.curr += 1;
        self.tokens.next()
    }

    fn expression(&mut self) -> Result<Box<Expr>> {
        self.equality()
    }

    // term >= term; term >= term
    fn equality(&mut self) -> Result<Box<Expr>> {
        let mut comparison = self.comparison()?;

        loop {
            match self.peek() {
                Some(&Token::Equal) | Some(&Token::BangEqual) => {
                    let op = match self.advance() {
                        Some(op) => op,
                        None => return Ok(Box::new(Expr::EOF))
                    };

                    let right = self.term()?;
                    comparison = Box::new(Expr::Binary(comparison, op, right));
                },
                _ => break,
            }
        } 

        Ok(comparison)
    }

    fn comparison(&mut self) -> Result<Box<Expr>> {
        let mut term = self.term()?;
        loop {
            match self.peek() {
                Some(&Token::Greater) | Some(&Token::GreaterEqual) | Some(Token::Less) | Some(Token::LessEqual) => {
                    let op = match self.advance() {
                        Some(op) => op,
                        None => return Ok(Box::new(Expr::EOF))
                    };

                    let right = self.term()?;
                    term = Box::new(Expr::Binary(term, op, right))
                }
                _ => break,
            }
        }

        Ok(term)
    }

    fn term(&mut self) -> Result<Box<Expr>> {
        todo!()
    }
}