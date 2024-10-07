use miette::Diagnostic;
use thiserror::Error;

use crate::{
    generator::{Generator, GeneratorError},
    source::Source,
    symbol_table,
};

#[derive(Error, Diagnostic, Debug)]
pub enum CompilerError {
    #[error(transparent)]
    GeneratorError(
        #[from]
        #[diagnostic_source]
        GeneratorError,
    ),

    #[error(transparent)]
    IoError(#[from] std::io::Error),
}

pub struct Compiler<'iso, W>
where
    W: std::io::Write,
{
    generator: Generator<'iso>,
    out: W,
    symbol_table: std::rc::Rc<std::cell::RefCell<symbol_table::SymbolTable<'iso>>>,
}

impl<'iso, W> Compiler<'iso, W>
where
    W: std::io::Write,
{
    pub fn new(src: &'iso Source, out: W) -> Result<Self, CompilerError> {
        let symbol_table =
            std::rc::Rc::new(std::cell::RefCell::new(symbol_table::SymbolTable::default()));

        Ok(Self {
            generator: Generator::new(src, std::rc::Rc::clone(&symbol_table))?,
            out,
            symbol_table,
        })
    }

    pub fn compile(mut self /* TODO: options */) -> Result<(), CompilerError> {
        for statement in self.generator {
            let statement = statement?;

            writeln!(self.out, "{}", statement)?;
        }

        Ok(())
    }
}
