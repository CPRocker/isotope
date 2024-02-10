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
}
