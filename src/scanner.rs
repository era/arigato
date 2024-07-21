//TODO move from here
#[derive(Debug, PartialEq)]
pub enum Token {
    // single character
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    Assign,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    Text,
    Number,

    // Keywords
    And,
    Class,
    If,
    Else,
    False,
    True,
    Fun,
    Nil,
    Or,
    Return,
    Super,
    This,
    Var,
    While,
    Print,

    Eof,
}

//TODO add user-friendly message by implementing Debug manually
#[derive(Debug, PartialEq)]
pub struct LexicalError {
    pub line: usize,
    pub col: usize,
    pub description: &'static str,
}

pub struct Scanner<I>
where
    I: Iterator<Item = char>,
{
    input: I,
    // tells if our current scanner found errors.
    had_error: bool,
    curr_lexeme_start: usize,
    curr_pos: usize,
    curr_line: usize,
}

/// Scanner is responsible for reading a string and returning
/// all tokens from it. If there is an error, it will try to keep
/// parsing as long as possible in order to return all possible errors.
impl<I: std::iter::Iterator<Item = char>> Scanner<I> {
    pub fn new(input: I) -> Self {
        Self {
            input: input,
            curr_lexeme_start: 0,
            curr_pos: 0,
            had_error: false,
            curr_line: 1,
        }
    }

    pub fn scan(&mut self) -> Result<Vec<Token>, Vec<LexicalError>> {
        let mut tokens = vec![];
        let mut errors = vec![];
        while let Some(token) = self.next() {
            match token {
                Ok(_) => todo!(),
                Err(e) => errors.push(e),
            };
        }
        tokens.push(Token::Eof);

        if errors.is_empty() {
            return Ok(tokens);
        } else {
            return Err(errors);
        }
    }

    fn next(&mut self) -> Option<Result<Token, LexicalError>> {
        let c = self.advance();
        //TODO improve this
        if let None = c {
            return None;
        }

        let token = match c.unwrap() {
            '{' => Ok(Token::LeftBrace),
            _ => Err(LexicalError {
                line: self.curr_line,
                col: self.curr_pos, //TODO not sure
                description: "Unexpected character",
            }),
        };

        Some(token)
    }

    /// moves towards the next token, it can move
    /// more than one char at time.
    fn advance(&mut self) -> Option<char> {
        self.curr_pos += 1;
        self.input.nth(self.curr_pos)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn empty_string() {
        let mut scanner = Scanner::new("".chars());
        assert_eq!(Ok(vec![Token::Eof]), scanner.scan());
    }

    //TODO create table test
    #[test]
    pub fn scanner_with_error() {
        let mut scanner = Scanner::new("--".chars());
        assert_eq!(
            Err(vec![LexicalError {
                line: 1,
                col: 1,
                description: "Unexpected character" //TODO avoid spreading this string
            }]),
            scanner.scan()
        );
    }
}
