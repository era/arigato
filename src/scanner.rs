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
    Comment,
    Space,

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

const UNEXPECTED_CHAR: &str = "unexpected character";

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
            input,
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
                Ok(Token::Space) | Ok(Token::Comment) => continue,
                Ok(_) => todo!(),
                Err(e) => errors.push(e),
            };
        }
        tokens.push(Token::Eof);

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn next(&mut self) -> Option<Result<Token, LexicalError>> {
        let token = match self.advance()? {
            '{' => Ok(Token::LeftBrace),
            '}' => Ok(Token::RightBrace),
            '(' => Ok(Token::LeftParen),
            ')' => Ok(Token::RightParen),
            ',' => Ok(Token::Comma),
            '.' => Ok(Token::Dot),
            '-' => Ok(Token::Minus),
            '+' => Ok(Token::Plus),
            ';' => Ok(Token::SemiColon),
            '*' => Ok(Token::Star),
            '!' if self.advance_if_match('=') => Ok(Token::BangEqual),
            '!' => Ok(Token::Bang),
            '=' if self.advance_if_match('=') => Ok(Token::Equal),
            '=' => Ok(Token::Assign),
            '<' if self.advance_if_match('=') => Ok(Token::LessEqual),
            '<' => Ok(Token::Less),
            '>' if self.advance_if_match('=') => Ok(Token::GreaterEqual),
            '>' => Ok(Token::Greater),
            '/' => {
                if self.advance_if_match('/') {
                    // this is a comment, we can ignore it.
                    // the comment is valid until the end of the line
                    while self.peek() == Some('\n') {
                        self.advance();
                    }
                    Ok(Token::Comment)
                } else {
                    Ok(Token::Slash)
                }
            }
            ' ' | '\r' | '\t' => Ok(Token::Space),
            '\n' => {
                self.curr_line += 1;
                Ok(Token::Space)
            }
            _ => Err(LexicalError {
                line: self.curr_line,
                col: self.curr_pos, //TODO not sure
                description: UNEXPECTED_CHAR,
            }),
        };

        Some(token)
    }

    // peek the next char without consuming it.
    fn peek(&mut self) -> Option<char> {
        self.input.nth(self.curr_pos)
    }

    /// moves towards the next token, it can move
    /// more than one char at time.
    fn advance(&mut self) -> Option<char> {
        let c = self.input.nth(self.curr_pos);
        self.curr_pos += 1;
        c
    }

    // only consumes char if it matches the expected value.
    fn advance_if_match(&mut self, expected: char) -> bool {
        match self.input.nth(self.curr_pos) {
            None => false,
            Some(c) if c == expected => {
                self.curr_pos += 1;
                true
            }
            Some(_) => false,
        }
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

    //TODO create table test, also this test does not make sense
    #[test]
    pub fn scanner_with_error() {
        let mut scanner = Scanner::new("c".chars());
        assert_eq!(
            Err(vec![LexicalError {
                line: 1,
                col: 1,
                description: UNEXPECTED_CHAR,
            }]),
            scanner.scan()
        );
    }
}
