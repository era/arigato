use itertools::PeekNth;
use itertools::peek_nth;

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
    Text(String), // "this is a valid string"
    Number(String), // 123 123.123 -123 -123.123

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
    input: PeekNth<I>,
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
            input: peek_nth(input),
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
                Ok(t) => tokens.push(t),
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
            '"' => self.string(),

            ' ' | '\r' | '\t' => Ok(Token::Space),
            '\n' => {
                self.new_line();
                Ok(Token::Space)
            }
            c@ '0'..='9' => self.digit(c),
            _ => Err(LexicalError {
                    line: self.curr_line,
                    col: self.curr_pos,
                    description: UNEXPECTED_CHAR,
                }),
        };

        Some(token)
    }

    fn new_line(&mut self) {
        self.curr_line += 1;
    }

    // peek the next char without consuming it.
    fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    fn peek_next(&mut self) -> Option<char> {
        self.input.peek_nth(1).copied()
    }

    /// moves towards the next token, it can move
    /// more than one char at time.
    fn advance(&mut self) -> Option<char> {
        self.curr_pos += 1;
        self.input.next()
    }

    // only consumes char if it matches the expected value.
    fn advance_if_match(&mut self, expected: char) -> bool {
        match self.input.next_if_eq(&expected) {
            None => false,
            Some(_) => {
                self.curr_pos += 1;
                true
            }

        }
    }

    // because we consume the first digit on the caller, we need to receive it as argument.
    fn digit(&mut self, first_digit: char) -> Result<Token, LexicalError> {
        let mut already_had_a_dot = false;
        let mut the_digit = vec![first_digit];

        while let Some(c) = self.peek() {
            // checking the next char as well
            // because if it's a dot we need to check
            // if the next char is a digit or not.
            match (c, self.peek_next()) {
                // the curr char is a dot, but we already handled it before
                // so we cannot have 123.123.13. So we should break.
                ('.', _) if already_had_a_dot => break,
                // the current char is a dot (the first one), and the next char is a digit
                // so this is a float.
                ('.', Some('0'..='9')) =>  {
                            already_had_a_dot = true;
                            the_digit.push(self.advance().unwrap());
                            continue
                        }
                // the current char is a digit, so we can just handle it.
                ('0'..='9', _) => {
                    the_digit.push(self.advance().unwrap());
                    continue
                }
                // No digits anymore, we should break
                _ => break,
            }
        }
        Ok(Token::Number(the_digit.iter().collect()))
    }

    fn string(&mut self) -> Result<Token, LexicalError> {
       let mut the_string = vec![];
       // while we don't meet the end of the string keep fetching the value.
        while self.peek() != Some('"') {

            if self.peek() == Some('\n') {
                self.new_line();
            }
            
            match self.advance() {
                Some(c) => the_string.push(c),
                None => {
                    return Err(LexicalError {
                    line: self.curr_line,
                    col: self.curr_pos,
                    description: "Expecting \""
                });
            }
            }
            
        }
        
        if self.peek() == Some('"') {
            self.advance();
        }
        
       Ok(Token::Text(the_string.into_iter().collect()))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn empty_string() {
        let mut scanner = Scanner::new("".chars().peekable());
        assert_eq!(Ok(vec![Token::Eof]), scanner.scan());
    }

    #[test]
    pub fn scanner_with_error() {
        let mut scanner = Scanner::new("c".chars().peekable());
        assert_eq!(
            Err(vec![LexicalError {
                line: 1,
                col: 1,
                description: UNEXPECTED_CHAR,
            }]),
            scanner.scan()
        );
    }

    #[test]
    pub fn scanner_a_string_literal() {
        let mut scanner = Scanner::new("\"this is a great string\"".chars());
        assert_eq!(
            Ok(vec![Token::Text("this is a great string".to_string()), Token::Eof]),
            scanner.scan()
        );
    }

    #[test]
    pub fn scanner_a_int() {
        let mut scanner = Scanner::new("123".chars());
        assert_eq!(
            Ok(vec![Token::Number("123".to_string()), Token::Eof]),
            scanner.scan()
        );
    }

    #[test]
    pub fn scanner_a_float() {
        let mut scanner = Scanner::new("103.129".chars());
        assert_eq!(
            Ok(vec![Token::Number("103.129".to_string()), Token::Eof]),
            scanner.scan()
        );
    }

    #[test]
    pub fn scanner_a_float_with_single_digit() {
        let mut scanner = Scanner::new("103.1".chars());
        assert_eq!(
            Ok(vec![Token::Number("103.1".to_string()), Token::Eof]),
            scanner.scan()
        );
    }

    #[test]
    pub fn scanner_a_number_with_a_dot() {
        // TODO this test in the future should throw an error, so we will need to fix it.
        let mut scanner = Scanner::new("123. 123".chars());
        assert_eq!(
            Ok(vec![Token::Number("123".to_string()), Token::Dot, Token::Number("123".to_string()), Token::Eof]),
            scanner.scan()
        );
    }
}