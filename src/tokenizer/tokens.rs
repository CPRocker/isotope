#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // compiler-time
    Eof,
    // single character
    Equals,
    LeftParen,
    RightParen,
    Minus,
    Plus,
    Slash,
    Star,
    Semi,
    // keywords
    Let,
    Return,
    // terminals
    Identifier(String),
    Literal(String),
}
