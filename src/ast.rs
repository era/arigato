use std::mem;

use crate::lang::Token;
use itertools::peek_nth;
use itertools::PeekNth;

#[derive(Debug)]
pub enum ParserError {
    Generic(&'static str),
}

pub enum Expr {
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Literal(Token),
    Identifier(Token),
    Grouping(Box<Expr>),
    EOF,
}

pub enum Statement {}

pub enum G {
    Statement(Statement),
    Expr(Expr)
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
    curr: usize,
}
// Recursive Descent Parsing
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
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

    pub fn parse(&mut self) -> Result<Vec<G>> {
        let mut g = vec![];
        while let Some(_) = self.peek() {
            g.push(G::Expr(*self.expression()?));
        }
        Ok(g)
    }

    // assignment
    fn expression(&mut self) -> Result<Box<Expr>> {
        self.equality()
    }

    // comparison ( ( "!=" | "==" ) comparison )*
    fn equality(&mut self) -> Result<Box<Expr>> {
        let mut comparison = self.comparison()?;

        loop {
            match self.peek() {
                Some(&Token::Equal) | Some(&Token::BangEqual) => {
                    let op = match self.advance() {
                        Some(op) => op,
                        None => return Ok(Box::new(Expr::EOF)),
                    };

                    let right = self.term()?;
                    comparison = Box::new(Expr::Binary(comparison, op, right));
                }
                _ => break,
            }
        }

        Ok(comparison)
    }

    // term ( ( ">" | ">=" | "<" | "<=" ) term )*
    fn comparison(&mut self) -> Result<Box<Expr>> {
        let mut term = self.term()?;
        loop {
            match self.peek() {
                Some(&Token::Greater)
                | Some(&Token::GreaterEqual)
                | Some(Token::Less)
                | Some(Token::LessEqual) => {
                    let op = match self.advance() {
                        Some(op) => op,
                        None => return Ok(Box::new(Expr::EOF)),
                    };

                    let right = self.term()?;
                    term = Box::new(Expr::Binary(term, op, right))
                }
                _ => break,
            }
        }

        Ok(term)
    }

    // factor ( ( "-" | "+" ) factor )*
    fn term(&mut self) -> Result<Box<Expr>> {
        let mut factor = self.factor()?;
        loop {
            match self.peek() {
                Some(&Token::Minus) | Some(&Token::Plus) => {
                    let op = self.advance().unwrap();
                    let right = self.factor()?;
                    factor = Box::new(Expr::Binary(factor, op, right))
                }
                _ => break,
            }
        }

        Ok(factor)
    }

    // unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Box<Expr>> {
        let mut unary = self.unary()?;
        loop {
            match self.peek() {
                Some(&Token::Slash) | Some(&Token::Star) => {
                    let op = self.advance().unwrap();
                    let right = self.unary()?;
                    unary = Box::new(Expr::Binary(unary, op, right))
                }
                _ => break,
            }
        }
        Ok(unary)
    }

    // ( "!" | "-" ) unary | call ;
    fn unary(&mut self) -> Result<Box<Expr>> {
        match self.peek() {
            Some(&Token::Bang) | Some(&Token::Minus) => {
                let op = self.advance().unwrap();
                let unary = self.unary()?;
                Ok(Box::new(Expr::Unary(op, unary)))
            }
            Some(_) => self.primary(),
            None => Ok(Box::new(Expr::EOF)),
        }
    }

    // "true" | "false" | "nil" | "this"
    // | NUMBER | STRING | IDENTIFIER | "(" expression ")"
    // | "super" "." IDENTIFIER ;
    fn primary(&mut self) -> Result<Box<Expr>> {
        match self.peek() {
            // literals
            Some(&Token::True)
            | Some(&Token::False)
            | Some(&Token::Nil)
            | Some(&Token::Number(_))
            | Some(&Token::Text(_)) => Ok(Box::new(Expr::Literal(self.advance().unwrap()))),
            // identifiers FIXME
            Some(&Token::This) | Some(&Token::Identifier(_)) | Some(&Token::Super) => {
                Ok(Box::new(Expr::Identifier(self.advance().unwrap())))
            }

            // braces expression
            Some(&Token::LeftBrace) => {
                self.advance();
                let expr = self.expression()?;

                if let Some(Token::RightParen) = self.advance() {
                    Ok(Box::new(Expr::Grouping(expr)))
                } else {
                    Err(ParserError::Generic("expecting ')'"))
                }
            }
            _ => Err(ParserError::Generic(
                "error while processing primary. Unexpected token",
            )),
        }
    }
    // when there is an error, we enter "panic mode" and we need to go to the next
    // possible working statement. This is what synchronize is about.
    fn synchronize(&mut self) {
        while let Some(token) = self.advance() {
            match token {
                // if we are at a semicolon it means we for sure passed the last problem
                Token::SemiColon => break,
                Token::Class => break,
                Token::Fun => break,
                Token::Var => break,
                Token::For => break,
                Token::While => break,
                Token::Print => break,
                Token::Return => break,
                _ => continue,
            }
        }
    }
}
