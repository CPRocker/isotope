use crate::tokenizer::tokens::Token;

pub mod statements;

pub fn parse(tokens: Vec<Token>) -> tree::Tree<statements::Statement> {
    let mut t = tree::Tree::new();
    let program = t.new_node(statements::Statement::Program, None);
    t.set_root(Some(program));

    for token in tokens {
        dbg!(token);
    }

    t
}
