use miette::Diagnostic;
use thiserror::Error;

use crate::generator::{Generator, GeneratorError};

#[derive(Error, Diagnostic, Debug)]
pub enum CompilerError {
    #[error(transparent)]
    GeneratorError(
        #[from]
        #[diagnostic_source]
        GeneratorError,
    ),
}

pub struct Compiler<'iso, R, W>
where
    R: std::io::BufRead,
    W: std::io::Write,
{
    generator: Generator<'iso, R, W>,
}

impl<'de, R, W> Compiler<'de, R, W>
where
    R: std::io::BufRead,
    W: std::io::Write,
{
    pub fn new(src: &'de mut R, out: W) -> Self {
        Self {
            generator: Generator::new(src, out),
        }
    }

    pub fn compile(&mut self /* TODO: options */) -> Result<(), CompilerError> {
        self.generator
            .generate()
            .map_err(CompilerError::GeneratorError)
    }
}
