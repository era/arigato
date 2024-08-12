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

    fn evaluate(mut self, expr: Expr) -> Result<Type, Error> {
        match expr {
            Expr::Literal(token) => self.literal_expr(token),
            Expr::Grouping(expr) => self.grouping_expr(*expr),
            _ => todo!(),
        }
    }

    fn literal_expr(mut self, token: Token) -> Result<Type, Error> {
        match token {
            Token::Number(n) => Ok(Type::Number(n)),
            Token::Text(t) => Ok(Type::Text(t)),
            _ => Err(Error::UnexpectedExpr("expecting only literals"))
        }
    }

    fn grouping_expr(mut self, g: Expr) -> Result<Type, Error> {
         return self.evaluate(g)
    }
}