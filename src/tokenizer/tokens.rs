#[derive(Debug)]
pub enum Token {
    EOF,
    Identifier(String),
    Literal(String),
    Return,
    Semi,
}
