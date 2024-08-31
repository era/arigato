use std::mem;

use crate::lang::Token;
use itertools::peek_nth;
use itertools::PeekNth;

#[derive(Debug)]
pub enum ParserError {
    Generic(&'static str),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Assign(String, Box<Expr>),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Literal(Token),
    Grouping(Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    EOF,
}

#[derive(Debug, Clone)]
pub enum Statement {
    //TODO stop using box in some places and not others
    VarDeclaration(String, Option<Box<Expr>>),
    Block(Vec<Statement>),
    IfStatement(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
    Expr(Expr),
}

pub enum UserError {
    Err(ParserError, String),
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

    pub fn parse(&mut self) -> std::result::Result<Vec<Statement>, Vec<ParserError>> {
        let mut g = vec![];
        let mut errors = vec![];
        while let Some(_) = self.peek() {
            match self.declaration() {
                Err(e) => {
                    self.synchronize();
                    errors.push(UserError::Err(
                        e,
                        format!("error on {} position", self.curr),
                    ));
                }
                Ok(t) => g.push(t),
            }
        }
        Ok(g)
    }

    fn block(&mut self) -> Result<Statement> {
        self.advance(); // consumes {
        let mut block_stmts = vec![];
        while let Some(t) = self.peek() {
            match t {
                Token::RightBrace => {
                    self.advance(); // consumes }
                    return Ok(Statement::Block(block_stmts));
                }

                _ => block_stmts.push(self.declaration()?),
            }
        }
        Err(ParserError::Generic("expecting }"))
    }

    fn declaration(&mut self) -> Result<Statement> {
        match self.peek() {
            Some(&Token::Var) => self.var_statement(),
            Some(&Token::LeftBrace) => self.block(),
            Some(&Token::Eof) => {
                self.advance();
                Ok(Statement::Expr(Expr::EOF))
            }
            Some(_) => self.statement(),
            _ => Err(ParserError::Generic("Not expecting end of input")),
        }
    }

    fn statement(&mut self) -> Result<Statement> {
        match self.peek() {
            Some(&Token::If) => return self.if_statement(),
            Some(&Token::While) => return self.while_statement(),
            Some(&Token::For) => return self.for_statement(),
            Some(&Token::Print) => return self.print_statement(),
            _ => return self.expression_statement(),
        }
    }

    fn advance_or_error(&mut self, token: Option<Token>, msg: &'static str) -> Result<()> {
        if self.advance() == token {
            Ok(())
        } else {
            Err(ParserError::Generic(msg))
        }
    }

    fn for_statement(&mut self) -> Result<Statement> {
        self.advance(); // consume for token
        self.advance_or_error(Some(Token::LeftParen), "expecting (")?;

        let intializer = match self.peek() {
            Some(&Token::SemiColon) => {
                self.advance();
                None
            }
            Some(&Token::Var) => Some(self.var_statement()?),
            _ => Some(self.expression_statement()?),
        };

        let condition = match self.peek() {
            Some(Token::SemiColon) => None,
            Some(_) => Some(*self.expression()?),
            _ => return Err(ParserError::Generic("not expecting the end of the file")),
        };

        self.advance_or_error(Some(Token::SemiColon), "expecting';")?;

        let increment = if let Some(&Token::RightParen) = self.peek() {
            None
        } else {
            Some(*self.expression()?)
        };

        self.advance_or_error(Some(Token::RightParen), "expecting )")?;
        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Statement::Block(vec![body, Statement::Expr(increment)]);
        }

        let condition = if let Some(condition) = condition {
            condition
        } else {
            Expr::Literal(Token::True)
        };

        Ok(Statement::While(condition, Box::new(body)))
    }

    fn while_statement(&mut self) -> Result<Statement> {
        self.advance(); // consume while token
        self.advance_or_error(Some(Token::LeftParen), "expecting (")?;
        let expr = self.expression()?;

        self.advance_or_error(Some(Token::RightParen), "expecting )")?;

        let block = self.statement()?;

        Ok(Statement::While(*expr, Box::new(block)))
    }

    fn if_statement(&mut self) -> Result<Statement> {
        self.advance(); // consume if token
        let condition = self.expression()?;
        let if_true = Box::new(self.statement()?);
        let if_false = match self.peek() {
            Some(&Token::Else) => {
                self.advance(); // consume else token
                Some(Box::new(self.statement()?))
            }
            _ => None,
        };

        Ok(Statement::IfStatement(*condition, if_true, if_false))
    }

    fn expression_statement(&mut self) -> Result<Statement> {
        let expr = self.expression()?;
        match self.advance() {
            Some(Token::SemiColon) => Ok(Statement::Expr(*expr)),
            _ => Err(ParserError::Generic("missing ;")),
        }
    }

    fn var_statement(&mut self) -> Result<Statement> {
        self.advance(); // consumes VAR keyword
        let identifier_name = match self.advance() {
            Some(Token::Identifier(name)) => name,
            _ => return Err(ParserError::Generic("expecting an indentifier")),
        };

        let value = match self.peek() {
            Some(&Token::Assign) => {
                self.advance();
                Some(self.expression()?)
            }
            _ => None,
        };

        match self.peek() {
            Some(Token::SemiColon) => self.advance(),
            _ => return Err(ParserError::Generic("expecting a ;")),
        };

        Ok(Statement::VarDeclaration(identifier_name, value))
    }

    fn print_statement(&mut self) -> Result<Statement> {
        todo!()
    }

    fn expression(&mut self) -> Result<Box<Expr>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Box<Expr>> {
        let expr = self.or_expr()?;
        match (self.peek(), &*expr) {
            (Some(&Token::Equal), &Expr::Literal(Token::Identifier(ref id))) => {
                self.advance();
                //TODO avoid this clone
                Ok(Box::new(Expr::Assign(id.clone(), self.assignment()?)))
            }
            (Some(&Token::Assign), _) => Err(ParserError::Generic(
                "expecting identifier on left side of `=`",
            )),
            _ => Ok(expr),
        }
    }

    fn or_expr(&mut self) -> Result<Box<Expr>> {
        let mut expr = self.and_expr()?;
        while let Some(&Token::Or) = self.peek() {
            self.advance();
            expr = Box::new(Expr::Or(expr, self.and_expr()?));
        }

        Ok(expr)
    }

    fn and_expr(&mut self) -> Result<Box<Expr>> {
        let mut expr = self.equality()?;
        while let Some(&Token::And) = self.peek() {
            self.advance();
            expr = Box::new(Expr::Or(expr, self.equality()?));
        }

        Ok(expr)
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
                | Some(&Token::Less)
                | Some(&Token::LessEqual) => {
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
            | Some(&Token::Text(_))
            | Some(&Token::Identifier(_))
            | Some(&Token::This)
            | Some(&Token::Super) => Ok(Box::new(Expr::Literal(self.advance().unwrap()))),
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
            Some(&Token::Eof) => {
                self.advance();
                Ok(Box::new(Expr::EOF))
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
