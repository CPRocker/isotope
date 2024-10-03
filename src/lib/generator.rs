use miette::Diagnostic;
use thiserror::Error;

use crate::{
    parser::{Parser, ParserError, Stmt},
    source::Source,
};

#[derive(Error, Diagnostic, Debug)]
pub enum GeneratorError {
    #[error(transparent)]
    ParserError(
        #[from]
        #[diagnostic_source]
        ParserError,
    ),
    #[error(transparent)]
    IoError(#[from] std::io::Error),
}

pub struct Generator<'iso, W>
where
    W: std::io::Write,
{
    parser: Parser<'iso>,
    writer: W,
}

impl<'iso, W> Generator<'iso, W>
where
    W: std::io::Write,
{
    pub fn new(src: &'iso Source, out: W) -> Result<Self, GeneratorError> {
        Ok(Self {
            parser: Parser::new(src).map_err(GeneratorError::from)?,
            writer: out,
        })
    }

    pub fn generate(&mut self /* TODO: options */) -> Result<(), GeneratorError> {
        for statement in self.parser.by_ref() {
            let statement = statement.map_err(GeneratorError::ParserError)?;
            let ir = match statement {
                Stmt::Assignment { name, value } => todo!(),
                Stmt::Break => todo!(),
                Stmt::Expr(expr) => todo!(),
                Stmt::FunctionDeclaration { name, params, body } => todo!(),
                Stmt::If {
                    condition,
                    body,
                    else_body,
                } => todo!(),
                Stmt::LetDeclaration { name, value } => todo!(),
                Stmt::Loop { body } => todo!(),
                Stmt::Nop => "nop".to_string(),
                Stmt::Return(expr) => todo!(),
            };
            writeln!(self.writer, "{}", ir).map_err(GeneratorError::IoError)?;
        }

        self.writer.flush().map_err(GeneratorError::IoError)
    }
}
