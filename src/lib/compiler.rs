use miette::Diagnostic;
use thiserror::Error;

use crate::{
    generator::{Generator, GeneratorError},
    source::Source,
};

#[derive(Error, Diagnostic, Debug)]
pub enum CompilerError {
    #[error(transparent)]
    GeneratorError(
        #[from]
        #[diagnostic_source]
        GeneratorError,
    ),
}

pub struct Compiler<'iso, W>
where
    W: std::io::Write,
{
    generator: Generator<'iso, W>,
}

impl<'iso, W> Compiler<'iso, W>
where
    W: std::io::Write,
{
    pub fn new(src: &'iso Source, out: W) -> Result<Self, CompilerError> {
        Ok(Self {
            generator: Generator::new(src, out).map_err(CompilerError::from)?,
        })
    }

    pub fn compile(&mut self /* TODO: options */) -> Result<(), CompilerError> {
        self.generator
            .generate()
            .map_err(CompilerError::GeneratorError)
    }
}
