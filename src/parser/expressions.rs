// use crate::tokenizer::tokens::Token;

#[derive(Debug)]
pub enum Expression {
    // Binary(Box<Expression>, Token, Box<Expression>),
    Literal(Literal),
}

#[derive(Debug)]
pub enum Literal {
    IntLiteral { value: i64 },
}
