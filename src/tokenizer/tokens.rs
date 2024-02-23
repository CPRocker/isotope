#[derive(Debug)]
pub enum Token {
    // compiler-time
    Eof,
    // single character
    LeftParen,
    RightParen,
    Minus,
    Plus,
    Slash,
    Star,
    Semi,
    // keywords
    Return,
    // terminals
    Identifier(String),
    Literal(String),
}
