use miette::Diagnostic;
use thiserror::Error;

use crate::parser::{Parser, ParserError, Stmt};

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

pub struct Generator<'iso, R, W>
where
    R: std::io::BufRead,
    W: std::io::Write,
{
    parser: Parser<'iso, R>,
    writer: W,
}

impl<'iso, R, W> Generator<'iso, R, W>
where
    R: std::io::BufRead,
    W: std::io::Write,
{
    pub fn new(src: &'iso mut R, out: W) -> Self {
        Self {
            parser: Parser::new(src),
            writer: out,
        }
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
