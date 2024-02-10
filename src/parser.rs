use crate::tokenizer::tokens::Token;

pub mod statements;

use self::statements::Statement;

pub fn parse(tokens: Vec<Token>) -> tree::Tree<statements::Statement> {
    let mut t = tree::Tree::new(Some(Statement::Program));

    for token in tokens {
        dbg!(token);
    }

    t
}
