mod error;
mod generator;
mod parser;
mod tokenizer;

use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() -> Result<(), error::Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(error::Error::Usage);
    }

    let input_file_path_arg = args.get(1).unwrap();
    let isotope_file_path = Path::new(input_file_path_arg);
    if isotope_file_path.is_dir() {
        return Err(error::Error::Usage);
    }
    if isotope_file_path.extension().is_none()
        || (isotope_file_path.extension().unwrap() != "isotope"
            && isotope_file_path.extension().unwrap() != "⚛️")
    {
        return Err(error::Error::FileExtension);
    }

    let contents = fs::read_to_string(isotope_file_path)?;

    let tokens = tokenizer::tokenize(contents)?;

    let program = parser::parse(tokens.into())?;

    let output = generator::generate(program)?;

    let file_name = isotope_file_path.file_stem().unwrap().to_string_lossy();
    let asm_file_path = &format!("output/{}.asm", file_name);
    let obj_file_path = &format!("output/{}.obj", file_name);
    let exe_file_path = &format!("output/{}.exe", file_name);

    let _ = fs::write(asm_file_path, output);

    if cfg!(target_os = "windows") {
        assemble_windows(asm_file_path, obj_file_path)?;
        link_windows(obj_file_path, exe_file_path)?;
    } else {
        todo!();
    };

    Ok(())
}

fn assemble_windows(asm_file_path: &String, obj_file_path: &String) -> Result<(), error::Error> {
    Command::new("nasm")
        .args(["-f", "win64", "-o", obj_file_path, asm_file_path])
        .spawn()?;

    Ok(())
}

fn link_windows(obj_file_path: &String, exe_file_path: &String) -> Result<(), error::Error> {
    Command::new("gcc")
        .args([obj_file_path, "-o", exe_file_path, "-mconsole"])
        .spawn()?;

    Ok(())
}
