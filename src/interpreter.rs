use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use std::sync::Arc;

use crate::ast::Expr;
use crate::ast::Statement;
use crate::lang::Token;

#[derive(PartialEq, Clone)]
pub enum Type {
    Number(f64),
    Text(String),
    Bool(bool),
    Nil,
}

#[derive(Debug)]
pub enum Error {
    UnexpectedExpr(&'static str),
    NoSuchVariable,
    VariableNotInitialized,
}

#[derive(Clone)]
struct Environment {
    environment: HashMap<String, Option<Type>>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    fn new() -> Self {
        Self {
            environment: HashMap::new(),
            enclosing: None,
        }
    }

    fn new_from(env: Rc<RefCell<Environment>>) -> Self {
        Self {
            environment: HashMap::new(),
            enclosing: Some(env),
        }
    }

    fn define(&mut self, identifier: String, val: Option<Type>) {
        self.environment.insert(identifier, val);
    }

    fn assign(&mut self, identifier: String, val: Option<Type>) -> Result<(), Error> {
        match self.environment.get(&identifier) {
            None => {
                if let Some(ref mut e) = self.enclosing {
                    e.borrow_mut().assign(identifier, val)?;
                }
            }
            Some(_) => {
                self.define(identifier, val);
            }
        }
        Err(Error::NoSuchVariable)
    }

    fn get(&self, identifier: &str) -> Result<Option<Type>, Error> {
        match self.environment.get(identifier) {
            None => {
                if let Some(e) = &self.enclosing {
                    e.borrow_mut().get(identifier)
                } else {
                    Err(Error::NoSuchVariable)
                }
            }
            Some(t) => Ok(t.clone()),
        }
    }
}

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

// a tree-walking interpreter implemented by using the visitor pattern.
impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn interprete(&mut self, program: Vec<Statement>) -> Result<(), Error> {
        for s in program {
            self.evaluate_stmt(s)?;
        }
        Ok(())
    }

    fn evaluate_stmt(&mut self, stmt: Statement) -> Result<(), Error> {
        match stmt {
            Statement::IfStatement(condition, if_true, if_false) => {
                self.evaluate_if(condition, if_true, if_false)
            }
            Statement::VarDeclaration(id, value) => self.var_declaration(id, value),
            Statement::Block(block) => self.block_stmt(block),
            Statement::Expr(expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
        }
    }

    fn var_declaration(&mut self, identifier: String, val: Option<Box<Expr>>) -> Result<(), Error> {
        let value = val.map(|v| self.evaluate(*v));

        match value {
            Some(Err(e)) => return Err(e),
            Some(Ok(v)) => self.environment.borrow_mut().define(identifier, Some(v)),
            None => self.environment.borrow_mut().define(identifier, None),
        };
        Ok(())
    }

    fn evaluate(&mut self, expr: Expr) -> Result<Type, Error> {
        match expr {
            Expr::Literal(token) => self.literal_expr(token),
            Expr::Grouping(expr) => self.grouping_expr(*expr),
            Expr::Unary(t, e) => self.unary_expr(t, *e),
            Expr::Binary(l, t, r) => self.binary(*l, *r, t),
            Expr::EOF => Ok(Type::Nil),
            Expr::Assign(id, value) => self.assign_expr(id, *value),
        }
    }

    fn block_stmt(&mut self, statements: Vec<Statement>) -> Result<(), Error> {
        let env = Rc::new(RefCell::new(Environment::new_from(
            self.environment.clone(),
        )));
        self.execute_block(statements, env)
    }

    fn execute_block(
        &mut self,
        stmts: Vec<Statement>,
        mut env: Rc<RefCell<Environment>>,
    ) -> Result<(), Error> {
        mem::swap(&mut env, &mut self.environment);
        for s in stmts {
            self.evaluate_stmt(s)?;
        }
        mem::swap(&mut env, &mut self.environment);
        Ok(())
    }

    fn literal_expr(&mut self, token: Token) -> Result<Type, Error> {
        match token {
            Token::Number(n) => {
                Ok(Type::Number(n.parse().map_err(|_| {
                    Error::UnexpectedExpr("expecting a number")
                })?))
            }
            Token::Text(t) => Ok(Type::Text(t)),
            Token::Identifier(id) => {
                if let Some(t) = self.environment.borrow_mut().get(&id)? {
                    Ok(t)
                } else {
                    Err(Error::VariableNotInitialized)
                }
            }
            _ => Err(Error::UnexpectedExpr("expecting only literals")),
        }
    }

    fn put_value(&mut self, id: String, value: Type) -> Result<(), Error> {
        self.environment.borrow_mut().assign(id, Some(value))
    }

    fn assign_expr(&mut self, id: String, value: Expr) -> Result<Type, Error> {
        let value = self.evaluate(value)?;
        self.put_value(id, value.clone())?;
        Ok(value)
    }

    fn grouping_expr(&mut self, g: Expr) -> Result<Type, Error> {
        return self.evaluate(g);
    }

    fn evaluate_if(
        &mut self,
        condition: Expr,
        if_true: Box<Statement>,
        if_false: Option<Box<Statement>>,
    ) -> Result<(), Error> {
        match (self.evaluate(condition)?, if_false) {
            (Type::Bool(true), _) => self.evaluate_stmt(*if_true),
            (Type::Bool(false), Some(if_false)) => self.evaluate_stmt(*if_false),
            (_, _) => Err(Error::UnexpectedExpr(
                "expecting condition to evaluate to boolean",
            )),
        }
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
                _ => Err(Error::UnexpectedExpr(
                    "expecting a text or a number on both sides of op",
                )),
            },
            Token::Minus => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Number(l - r)),
                _ => Err(Error::UnexpectedExpr(
                    "expecting a number on both sides of op",
                )),
            },
            Token::Slash => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Number(l / r)),
                _ => Err(Error::UnexpectedExpr(
                    "expecting a number on both sides of op",
                )),
            },
            Token::Star => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Number(l * r)),
                _ => Err(Error::UnexpectedExpr(
                    "expecting a number on both sides of op",
                )),
            },
            Token::Equal => Ok(Type::Bool(left == right)),
            Token::BangEqual => Ok(Type::Bool(left != right)),
            Token::Less => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Bool(l < r)),
                _ => Err(Error::UnexpectedExpr(
                    "expecting a number on both sides of op",
                )),
            },
            Token::LessEqual => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Bool(l <= r)),
                _ => Err(Error::UnexpectedExpr(
                    "expecting a number on both sides of op",
                )),
            },
            Token::Greater => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Bool(l > r)),
                _ => Err(Error::UnexpectedExpr(
                    "expecting a number on both sides of op",
                )),
            },
            Token::GreaterEqual => match (left, right) {
                (Type::Number(l), Type::Number(r)) => Ok(Type::Bool(l >= r)),
                _ => Err(Error::UnexpectedExpr(
                    "expecting a number on both sides of op",
                )),
            },
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{ast::Parser, scanner::Scanner};

    #[test]
    pub fn test_var_declaration() {
        let tokens = Scanner::new("var a;".chars()).scan().unwrap();
        let ast = Parser::new(tokens).parse().unwrap();
        Interpreter::new().interprete(ast).unwrap();
    }

    #[test]
    pub fn test_var_assign() {
        let tokens = Scanner::new("var a = 1;".chars()).scan().unwrap();
        let ast = Parser::new(tokens).parse().unwrap();
        Interpreter::new().interprete(ast).unwrap();
    }

    #[test]
    pub fn test_blocks() {
        let tokens = Scanner::new("var a = 1; {var b = a;}".chars())
            .scan()
            .unwrap();
        let ast = Parser::new(tokens).parse().unwrap();
        Interpreter::new().interprete(ast).unwrap();
    }
}
