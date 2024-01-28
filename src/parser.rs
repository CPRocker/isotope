use crate::tokenizer::tokens::Token;

pub mod statements;

pub fn parse(tokens: Vec<Token>) -> Vec<statements::Statement> {
    return vec!(statements::Statement {});
}