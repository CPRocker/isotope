use clap::{Parser, ValueEnum};
use miette::{Diagnostic, IntoDiagnostic, Result};
use thiserror::Error;

use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
#[clap(name = "Isotope Compiler")]
struct Cli {
    #[clap(value_enum)]
    command: Command,

    #[clap()]
    file_path: PathBuf,
}

#[derive(ValueEnum, Clone)]
enum Command {
    Lex,
    Parse,
    Generate,
    Compile,
}

#[derive(Error, Diagnostic, Debug)]
pub enum CliError {
    #[error("Invalid file extension: {file_extension}")]
    #[diagnostic(help(
        "The file extension must match one of the following: .isotope, .tope, or .⚛️."
    ))]
    InvalidExtensionError { file_extension: String },
}

fn main() -> Result<()> {
    let args = Cli::parse();

    let file_path = args.file_path.to_str().unwrap_or_default().to_string();

    let file_extension = args
        .file_path
        .extension()
        .map_or("", |ext| ext.to_str().unwrap_or_default());
    if !matches!(file_extension, "isotope" | "tope" | "⚛️") {
        return Err(CliError::InvalidExtensionError {
            file_extension: file_extension.to_string(),
        }
        .into());
    }

    let file = fs::File::open(file_path).into_diagnostic()?;
    let source = isotope::source::Source::from_reader(file).into_diagnostic()?;

    let stdout = std::io::stdout();
    let stdout = stdout.lock();

    match args.command {
        Command::Lex => lex(source),
        Command::Parse => parse(source),
        Command::Generate => generate(source),
        Command::Compile => compile(source, stdout),
    }
}

fn lex(src: isotope::source::Source) -> Result<()> {
    let symbol_table = std::rc::Rc::new(std::cell::RefCell::new(
        isotope::symbol_table::SymbolTable::default(),
    ));
    let lexer = isotope::lexer::Lexer::new(&src, std::rc::Rc::clone(&symbol_table))?;

    for token in lexer {
        let token = token
            .into_diagnostic()
            .map_err(|e| e.with_source_code(src.clone()))?;
        println!("{:?}", token);
    }

    Ok(())
}

fn parse(src: isotope::source::Source) -> Result<()> {
    let symbol_table = std::rc::Rc::new(std::cell::RefCell::new(
        isotope::symbol_table::SymbolTable::default(),
    ));
    let parser = isotope::parser::Parser::new(&src, std::rc::Rc::clone(&symbol_table))?;

    for statement in parser {
        let statement = statement
            .into_diagnostic()
            .map_err(|e| e.with_source_code(src.clone()))?;
        println!("{}", statement);
    }

    Ok(())
}

fn generate(src: isotope::source::Source) -> Result<()> {
    let symbol_table = std::rc::Rc::new(std::cell::RefCell::new(
        isotope::symbol_table::SymbolTable::default(),
    ));
    let generator = isotope::generator::Generator::new(&src, std::rc::Rc::clone(&symbol_table))?;

    for statement in generator {
        let statement = statement
            .into_diagnostic()
            .map_err(|e| e.with_source_code(src.clone()))?;
        println!("{}", statement);
    }

    Ok(())
}

fn compile<W: std::io::Write>(src: isotope::source::Source, out: W) -> Result<()> {
    // TODO: decide on compiler backend based on machine
    let compiler = isotope::compiler::Compiler::new(&src, out)?;

    compiler
        .compile()
        .into_diagnostic()
        .map_err(|e| e.with_source_code(src.clone()))?;

    Ok(())
}

// fn run() -> Result<(), lib::error::Error> {
//     let args: Vec<String> = env::args().collect();
//     if args.len() != 2 {
//         return Err(lib::error::Error::Usage);
//     }

//     let input_file_path_arg = args.get(1).unwrap();
//     let isotope_file_path = Path::new(input_file_path_arg);
//     if isotope_file_path.is_dir() {
//         return Err(error::Error::Usage);
//     }
//     if isotope_file_path.extension().is_none()
//         || (isotope_file_path.extension().unwrap() != "isotope"
//             && isotope_file_path.extension().unwrap() != "⚛️")
//     {
//         return Err(error::Error::FileExtension);
//     }

//     let contents = fs::read_to_string(isotope_file_path)?;

//     let tokens = tokenizer::tokenize(contents)?;

//     let program = parser::parse(tokens.into())?;

//     let output = generator::generate(program)?;

//     let file_name = isotope_file_path.file_stem().unwrap().to_string_lossy();
//     let asm_file_path = &format!("output/{}.asm", file_name);
//     let obj_file_path = &format!("output/{}.obj", file_name);
//     let exe_file_path = &format!("output/{}.exe", file_name);

//     let _ = fs::write(asm_file_path, output);

//     if cfg!(target_os = "windows") {
//         assemble_windows(asm_file_path, obj_file_path)?;
//         link_windows(obj_file_path, exe_file_path)?;
//     } else {
//         todo!();
//     };

//     Ok(())
// }

// fn assemble_windows(asm_file_path: &String, obj_file_path: &String) -> Result<(), error::Error> {
//     Command::new("nasm")
//         .args(["-f", "win64", "-o", obj_file_path, asm_file_path])
//         .spawn()?;

//     Ok(())
// }

// fn link_windows(obj_file_path: &String, exe_file_path: &String) -> Result<(), error::Error> {
//     Command::new("gcc")
//         .args([obj_file_path, "-o", exe_file_path, "-mconsole"])
//         .spawn()?;

//     Ok(())
// }
