use crate::tokenizer::tokens::Token;

pub mod statements;

pub fn parse(tokens: Vec<Token>) -> Vec<statements::Statement> {
    dbg!(tokens);

    vec![statements::Statement {}]
}
