mod generator;
mod parser;
mod tokenizer;

use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Usage: isotope `path`");
    }

    let file_path = args.get(1).unwrap();

    if !file_path.ends_with(".isotope") {
        panic!("File must end with `.isotope`");
    }

    let contents = fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Cannot read file: `{}`", file_path));

    let tokens = tokenizer::tokenize(contents);

    let statements = parser::parse(tokens);

    let output = generator::generate(statements);

    // TODO: write to output file
    println!("{}", output);

    let mut t = tree::Tree::default();

    let a = t.new_node(0, None);
    t.set_root(Some(a));
    let b = t.add_child(1, a);
    let _c = t.add_child(2, a);

    let _d = t.add_child(3, b);
    let _e = t.add_child(4, b);

    for value in t.into_iter() {
        dbg!(value);
    }
}
