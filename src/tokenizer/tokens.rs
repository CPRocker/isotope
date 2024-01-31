#[derive(Debug)]
pub enum Token {
    Identifier(String),
    Literal(String),
    Return,
    Semi,
}
