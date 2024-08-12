use crate::ast::Expr;
use crate::lang::Token;

pub enum Type {
    Number(String),
    Text(String),
}

pub enum Error {
    UnexpectedExpr(&'static str)
}

pub struct Interpreter {}

// a tree-walking interpreter implemented by using the visitor pattern.
impl Interpreter {
    fn visitLiteralExpr(lit: Expr) -> Result<Type, Error> {
        let token = match lit {
            Expr::Literal(token) => token,
            _ => return Err(Error::UnexpectedExpr("expecting only literals")),
        };
        match token {
            Token::Number(n) => Ok(Type::Number(n)),
            Token::Text(t) => Ok(Type::Text(t)),
            _ => Err(Error::UnexpectedExpr("expecting only literals"))
        }
    }
}