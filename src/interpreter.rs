use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

use crate::ast::Expr;
use crate::ast::Statement;
use crate::lang::Token;

#[derive(PartialEq, Clone, Debug)]
pub enum NativeFunction {
    Clock,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
    Number(f64),
    Text(String),
    Bool(bool),
    Callable(Vec<String>, Vec<Statement>, Environment),
    //TODO if we cannot find a Callable with identifier in env,
    // check if it's in a list of native function, if so, run it
    NativeFunction(NativeFunction, Vec<String>),
    Nil,
}

#[derive(Debug)]
pub enum Error {
    UnexpectedExpr(&'static str),
    WrongNumberOfArgs(&'static str),
    NoSuchVariable,
    NoSuchNativeFunction,
    VariableNotInitialized,
    // use to stop executing a function and return the value
    ReturnValue(Type),
}

#[derive(Clone, PartialEq, Debug)]
struct Environment {
    environment: HashMap<String, Option<Type>>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl NativeFunction {
    fn call(&self, args: Vec<Type>) -> Result<Type, Error> {
        match &self {
            NativeFunction::Clock => todo!(),
            _ => Err(Error::NoSuchNativeFunction),
        }
    }
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

    fn define_at(&mut self, distance: i64, identifier: String, val: Option<Type>) {
        self.ancestor(distance).borrow_mut().define(identifier, val)
    }

    fn ancestor(&mut self, distance: i64) -> Rc<RefCell<Environment>> {
        let mut ancestor = None;
        for _ in 0..distance {
            ancestor = self.enclosing.clone();
        }

        ancestor.expect("resolver found the wrong distance for the variable definition")
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

    fn get_at(&mut self, distance: i64, identifier: &str) -> Result<Option<Type>, Error> {
        self.ancestor(distance).borrow().get(identifier)
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
    globals: Rc<RefCell<Environment>>,
    locals: Option<HashMap<String, i64>>,
}

// a tree-walking interpreter implemented by using the visitor pattern.
impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));

        globals.borrow_mut().define(
            "clock".to_owned(),
            Some(Type::NativeFunction(NativeFunction::Clock, vec![])),
        );

        let environment = Rc::new(RefCell::new(Environment::new()));

        Self {
            environment,
            globals,
            locals: None,
        }
    }

    pub fn interprete(&mut self, program: Vec<Statement>) -> Result<(), Error> {
        let locals = Resolver::new().run(program.clone())?;
        self.locals = Some(locals);
        for s in program {
            self.evaluate_stmt(s)?;
        }
        Ok(())
    }

    fn evaluate_stmt(&mut self, stmt: Statement) -> Result<Option<Type>, Error> {
        match stmt {
            Statement::IfStatement(condition, if_true, if_false) => {
                self.evaluate_if(condition, if_true, if_false)?;
                Ok(None)
            }
            Statement::While(condition, block) => {
                self.while_eval(condition, *block)?;
                Ok(None)
            }
            Statement::VarDeclaration(id, value) => {
                self.var_declaration(id, value)?;
                Ok(None)
            }
            Statement::Block(block) => {
                self.block_stmt(block)?;
                Ok(None)
            }
            Statement::FnDeclaration(name, args, block) => {
                self.fun_declaration(name, args, block)?;
                Ok(None)
            }
            Statement::Return(exp) => {
                self.return_statement(exp)?;
                unreachable!()
            }
            Statement::Expr(expr) => Ok(Some(self.evaluate(expr)?)),
        }
    }

    fn return_statement(&mut self, expr: Expr) -> Result<Statement, Error> {
        let result = self.evaluate(expr)?;
        Err(Error::ReturnValue(result))
    }

    fn fun_declaration(
        &mut self,
        name: String,
        args: Vec<String>,
        block: Vec<Statement>,
    ) -> Result<(), Error> {
        // FIXME: Considering all the scope instead of the scope up to the function definition
        let closure = Environment::new_from(self.environment.clone());
        self.globals
            .borrow_mut()
            .define(name, Some(Type::Callable(args, block, closure)));

        Ok(())
    }

    fn while_eval(&mut self, condition: Expr, block: Statement) -> Result<(), Error> {
        loop {
            match self.evaluate(condition.clone())? {
                Type::Bool(true) => {
                    self.evaluate_stmt(block.clone())?;
                    ()
                }
                Type::Bool(false) => return Ok(()),
                _ => {
                    return Err(Error::UnexpectedExpr(
                        "expecting a boolean value for while condition",
                    ))
                }
            }
        }
    }

    fn define_var(&mut self, identifier: String, val: Option<Type>) -> Result<(), Error> {
        let distance = self.locals.as_ref().unwrap().get(&identifier);
        match distance {
            None => self.globals.borrow_mut().define(identifier, val),
            Some(t) => self
                .environment
                .borrow_mut()
                .define_at(t.to_owned(), identifier, val),
        }
        Ok(())
    }

    fn var_declaration(&mut self, identifier: String, val: Option<Box<Expr>>) -> Result<(), Error> {
        let value = val.map(|v| self.evaluate(*v));

        match value {
            Some(Err(e)) => Err(e),
            Some(Ok(v)) => self.define_var(identifier, Some(v)),
            None => self.define_var(identifier, None),
        }
    }

    fn evaluate(&mut self, expr: Expr) -> Result<Type, Error> {
        match expr {
            Expr::Call(callee, arguments) => self.call_function(*callee, arguments),
            Expr::Literal(token) => self.literal_expr(token),
            Expr::Grouping(expr) => self.grouping_expr(*expr),
            Expr::Unary(t, e) => self.unary_expr(t, *e),
            Expr::Binary(l, t, r) => self.binary(*l, *r, t),
            Expr::EOF => Ok(Type::Nil),
            Expr::Assign(id, value) => self.assign_expr(id, *value),
            Expr::Or(left, right) => self.logical_or(*left, *right),
            Expr::And(left, right) => self.logical_and(*left, *right),
        }
    }

    fn call_function(&mut self, callee: Expr, arguments: Vec<Expr>) -> Result<Type, Error> {
        match self.evaluate(callee)? {
            Type::Callable(args, stmts, closure) => {
                if args.len() != arguments.len() {
                    return Err(Error::WrongNumberOfArgs(
                        "arguments supplied did not match function signature",
                    ));
                }

                let closure = Rc::new(RefCell::new(closure));

                let mut env = Rc::new(RefCell::new(Environment::new_from(closure.clone())));

                for (i, arg) in args.iter().enumerate() {
                    let val = self.evaluate(arguments.get(i).unwrap().clone())?;
                    env.borrow_mut().define(arg.to_owned(), Some(val));
                }

                mem::swap(&mut env, &mut self.environment);
                let mut last_statement = None;
                for s in stmts {
                    last_statement = match self.evaluate_stmt(s) {
                        Err(Error::ReturnValue(t)) => return Ok(t),
                        Err(e) => return Err(e),
                        Ok(t) => t,
                    };
                }
                mem::swap(&mut env, &mut self.environment);

                Ok(last_statement.or_else(|| Some(Type::Nil)).unwrap())
            }
            _ => Err(Error::UnexpectedExpr("type is not callable")),
        }
    }

    fn logical_and(&mut self, left: Expr, right: Expr) -> Result<Type, Error> {
        let left = self.evaluate(left)?;
        if left == Type::Bool(false) {
            Ok(Type::Bool(false))
        } else {
            self.evaluate(right)
        }
    }

    fn logical_or(&mut self, left: Expr, right: Expr) -> Result<Type, Error> {
        let left = self.evaluate(left)?;
        if left == Type::Bool(true) {
            Ok(Type::Bool(true))
        } else {
            self.evaluate(right)
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
            Token::Identifier(id) => self.get_var(id),
            _ => Err(Error::UnexpectedExpr("expecting only literals")),
        }
    }

    fn get_var(&mut self, id: String) -> Result<Type, Error> {
        let distance = self.locals.as_ref().unwrap().get(&id);
        let val = match distance {
            None => self.globals.borrow().get(&id),
            Some(t) => self.environment.borrow_mut().get_at(t.to_owned(), &id),
        };

        match val {
            Ok(Some(v)) => Ok(v),
            Err(e) => Err(e),
            Ok(None) => Err(Error::VariableNotInitialized),
        }
    }

    fn put_value(&mut self, id: String, value: Type) -> Result<(), Error> {
        match self.environment.borrow().get(&id)? {
            Some(_) => self.environment.borrow_mut().assign(id, Some(value)),
            None => self.globals.borrow_mut().assign(id, Some(value)),
        }
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
            (Type::Bool(true), _) => {
                self.evaluate_stmt(*if_true)?;
                Ok(())
            }
            (Type::Bool(false), Some(if_false)) => {
                self.evaluate_stmt(*if_false)?;
                Ok(())
            }
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

pub struct Resolver {
    locals: HashMap<String, i64>,
    scopes: Vec<HashMap<String, bool>>,
}

impl Resolver {
    pub fn new() -> Self {
        let locals = HashMap::new();
        let scopes = Vec::new();

        Self { locals, scopes }
    }
    pub fn run(mut self, program: Vec<Statement>) -> Result<HashMap<String, i64>, Error> {
        self.resolve(program)?;

        Ok(self.locals)
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, id: String) -> Result<(), Error> {
        match self.scopes.last_mut() {
            None => Ok(()),
            Some(t) => {
                t.insert(id, false);
                Ok(())
            }
        }
    }

    fn define(&mut self, id: String) -> Result<(), Error> {
        match self.scopes.last_mut() {
            None => Ok(()),
            Some(t) => {
                t.insert(id, true);
                Ok(())
            }
        }
    }

    fn resolve(&mut self, stmts: Vec<Statement>) -> Result<(), Error> {
        for statement in stmts {
            self.resolve_stmt(statement)?;
        }
        Ok(())
    }
    fn resolve_stmt(&mut self, stmt: Statement) -> Result<(), Error> {
        match stmt {
            Statement::Expr(expr) => self.resolve_expr(expr)?,
            Statement::Block(stmts) => self.block_stmt(stmts)?,
            Statement::VarDeclaration(name, expr) => self.var_declaration(name, expr)?,
            Statement::FnDeclaration(name, params, body) => {
                self.function_stmt(name, params, body)?
            }
            Statement::IfStatement(condition, if_true, if_false) => {
                self.if_statement(condition, *if_true, if_false)?
            }
            Statement::Return(expr) => self.resolve_expr(expr)?,
            Statement::While(condition, statement) => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(*statement)?;
            }
        };
        Ok(())
    }

    fn resolve_expr(&mut self, expr: Expr) -> Result<(), Error> {
        match expr {
            Expr::Literal(l) => self.literal(l),
            Expr::Assign(name, expr) => self.assign_expr(name, *expr),
            Expr::Call(id, args) => self.call_expr(*id, args),
            Expr::Or(left, right) | Expr::And(left, right) | Expr::Binary(left, _, right) => {
                self.logical_op(*left, *right)
            }
            Expr::Unary(_, expr) | Expr::Grouping(expr) => self.resolve_expr(*expr),
            Expr::EOF => Ok(()),
        }
    }

    fn resolve_local(&mut self, id: &str) -> Result<(), Error> {
        for (i, v) in self.scopes.iter().enumerate() {
            if v.contains_key(id) {
                self.locals
                    .insert(id.to_owned(), (self.scopes.len() - 1 - i) as i64);
            }
        }

        Ok(())
    }

    fn logical_op(&mut self, left: Expr, right: Expr) -> Result<(), Error> {
        self.resolve_expr(left)?;
        self.resolve_expr(right)
    }

    fn call_expr(&mut self, id: Expr, args: Vec<Expr>) -> Result<(), Error> {
        self.resolve_expr(id)?;
        for arg in args {
            self.resolve_expr(arg)?;
        }

        Ok(())
    }

    fn if_statement(
        &mut self,
        condition: Expr,
        if_true: Statement,
        if_false: Option<Box<Statement>>,
    ) -> Result<(), Error> {
        self.resolve_expr(condition)?;
        self.resolve_stmt(if_true)?;
        if let Some(if_false) = if_false {
            self.resolve_stmt(*if_false)?;
        }

        Ok(())
    }

    fn function_stmt(
        &mut self,
        name: String,
        args: Vec<String>,
        stmts: Vec<Statement>,
    ) -> Result<(), Error> {
        self.declare(name.clone())?;
        self.define(name.clone())?;

        self.resolve_function(name, args, stmts)
    }

    fn resolve_function(
        &mut self,
        name: String,
        args: Vec<String>,
        stmts: Vec<Statement>,
    ) -> Result<(), Error> {
        self.begin_scope();
        for arg in args {
            self.declare(arg.clone())?;
            self.define(arg.clone())?;
        }
        self.resolve(stmts)?;
        self.end_scope();

        Ok(())
    }

    fn assign_expr(&mut self, name: String, expr: Expr) -> Result<(), Error> {
        self.resolve_expr(expr)?;

        self.resolve_local(&name)
    }

    fn block_stmt(&mut self, stmts: Vec<Statement>) -> Result<(), Error> {
        self.begin_scope();
        self.resolve(stmts)?;
        self.end_scope();

        Ok(())
    }

    fn var_declaration(&mut self, id: String, expr: Option<Box<Expr>>) -> Result<(), Error> {
        self.declare(id.clone())?;
        if let Some(e) = expr {
            self.resolve_expr(*e)?;
        }
        self.define(id)?;
        Ok(())
    }

    fn literal(&mut self, literal: Token) -> Result<(), Error> {
        let literal = match literal {
            Token::Identifier(name) => name,
            _ => return Ok(()),
        };
        if let Some(l) = self.scopes.last() {
            match l.get(&literal) {
                Some(false) => {
                    return Err(Error::UnexpectedExpr(
                        "can't read local variable in its own initializer.",
                    ))
                }
                _ => self.resolve_local(&literal)?,
            }
        }
        self.resolve_local(&literal)
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
