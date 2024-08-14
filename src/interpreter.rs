use crate::ast::Expr;
use crate::lang::Token;

#[derive(PartialEq)]
pub enum Type {
    Number(f64),
    Text(String),
    Bool(bool),
}

pub enum Error {
    UnexpectedExpr(&'static str),
}

pub struct Interpreter {}

// a tree-walking interpreter implemented by using the visitor pattern.
impl Interpreter {
    fn evaluate(&mut self, expr: Expr) -> Result<Type, Error> {
        match expr {
            Expr::Literal(token) => self.literal_expr(token),
            Expr::Grouping(expr) => self.grouping_expr(*expr),
            Expr::Unary(t, e) => self.unary_expr(t, *e),
            Expr::Binary(l, t, r) => self.binary(*l, *r, t),
            _ => todo!(),
        }
    }

    fn literal_expr(&mut self, token: Token) -> Result<Type, Error> {
        match token {
            Token::Number(n) => {
                Ok(Type::Number(n.parse().map_err(|_| {
                    Error::UnexpectedExpr("expecting a number")
                })?))
            }
            Token::Text(t) => Ok(Type::Text(t)),
            _ => Err(Error::UnexpectedExpr("expecting only literals")),
        }
    }

    fn grouping_expr(&mut self, g: Expr) -> Result<Type, Error> {
        return self.evaluate(g);
    }

    fn unary_expr(&mut self, token: Token, expr: Expr) -> Result<Type, Error> {
        match token {
            Token::Minus => match self.evaluate(expr)? {
                Type::Number(n) => Ok(Type::Number(-n)),
                _ => Err(Error::UnexpectedExpr("expecting only numbers")),
            },
            Token::Bang => match self.evaluate(expr)? {
                Type::Bool(b) => Ok(Type::Bool(!b)),
                _ => Err(Error::UnexpectedExpr("expecting only booleans")),
            },
            _ => unreachable!(),
        }
    }

    fn binary(&mut self, left: Expr, right: Expr, op: Token) -> Result<Type, Error> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;
        match op {
            Token::Plus => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Number(l + r)),
                (Type::Text(l), Type::Text(r)) => Ok(Type::Text(format!("{l}{r}"))),
                _ => Err(Error::UnexpectedExpr("expecting a text or a number on both sides of op"))
            }
            Token::Minus => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Number(l - r)),
                _ => Err(Error::UnexpectedExpr("expecting a number on both sides of op")),
            }
            Token::Slash => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Number(l / r)),
                _ => Err(Error::UnexpectedExpr("expecting a number on both sides of op")),
            },
            Token::Star => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Number(l * r)),
                _ => Err(Error::UnexpectedExpr("expecting a number on both sides of op")),
            },
            Token::Equal => Ok(Type::Bool(left == right)),
            Token::BangEqual => Ok(Type::Bool(left != right)),
            Token::Less => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Bool(l < r)),
                _ => Err(Error::UnexpectedExpr("expecting a number on both sides of op")),
            },
            Token::LessEqual => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Bool(l <= r)),
                _ => Err(Error::UnexpectedExpr("expecting a number on both sides of op")),
            },
            Token::Greater => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Bool(l > r)),
                _ => Err(Error::UnexpectedExpr("expecting a number on both sides of op")),
            },
            Token::GreaterEqual => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Bool(l >= r)),
                _ => Err(Error::UnexpectedExpr("expecting a number on both sides of op")),
            },
            _ => unreachable!(),
        }
    }
}
