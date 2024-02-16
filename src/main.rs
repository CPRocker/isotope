mod generator;
mod parser;
mod tokenizer;

use std::env;
use std::fs;
use std::process::Command;

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

    let program = parser::parse(tokens.into());
    let program = match program {
        Ok(program) => program,
        Err(e) => panic!("{}", e),
    };

    let output = generator::generate(program);
    let output_file_path = format!(
        "{}{}",
        file_path[..file_path.rfind('.').unwrap()].to_owned(),
        ".asm"
    );
    let _ = fs::write(output_file_path, output);

    if cfg!(target_os = "windows") {
        Command::new("nasm")
            .args(["-f", "win64", "examples\\file.asm"])
            .spawn()
            .expect("Failed to load assembler: `nasm`");
        Command::new("gcc")
            .args(["examples\\file.obj", "-mconsole"])
            .spawn()
            .expect("Failed to load linker: `gcc`");
    } else {
        todo!();
    };
}
