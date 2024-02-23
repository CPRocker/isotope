#[derive(Debug)]
pub enum Token {
    Eof,
    Identifier(String),
    Literal(String),
    Minus,
    Plus,
    Return,
    Semi,
    Slash,
    Star,
}
