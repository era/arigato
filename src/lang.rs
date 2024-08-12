
#[derive(Debug, PartialEq, Clone)]
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
    Identifier(String),
    Text(String), // "this is a valid string"
    Number(String), // 123 123.123

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
    For,
    Print,
    Extends,
    Export,
    External,
    Require,

    Eof,
}