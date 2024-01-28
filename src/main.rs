use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() <= 1 {
        panic!("Requires cmd line args to run");
    }

    let file_path = args.get(1)
        .expect("Requires file to compile");

    if !file_path.ends_with(".isotope") {
        panic!("File must end with `.isotope`");
    }

    let contents = fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Cannot read file: `{file_path}`"));

    dbg!(contents);
}
