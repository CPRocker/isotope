use miette::Diagnostic;
use thiserror::Error;

use crate::{
    parser::{Parser, ParserError, Statement},
    source::Source,
    symbol_table,
};

#[derive(Error, Diagnostic, Debug)]
pub enum GeneratorError {
    #[error(transparent)]
    ParserError(
        #[from]
        #[diagnostic_source]
        ParserError,
    ),
}

pub struct Generator<'iso> {
    parser: std::iter::Peekable<Parser<'iso>>,
    symbol_table: std::rc::Rc<std::cell::RefCell<symbol_table::SymbolTable>>,
}

impl<'iso> Generator<'iso> {
    pub fn new(
        src: &'iso Source,
        symbol_table: std::rc::Rc<std::cell::RefCell<symbol_table::SymbolTable>>,
    ) -> Result<Self, GeneratorError> {
        Ok(Self {
            parser: Parser::new(src, std::rc::Rc::clone(&symbol_table))?.peekable(),
            symbol_table: std::rc::Rc::clone(&symbol_table),
        })
    }
}

impl<'iso> std::iter::Iterator for Generator<'iso> {
    type Item = Result<String, GeneratorError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parser.peek()?;
        Some(self.generate_statement())
    }
}

impl<'iso> Generator<'iso> {
    fn generate_statement(&mut self) -> Result<String, GeneratorError> {
        match self.parser.next().expect("peek already checked")? {
            // TODO: need symbol table that tracks variables
            Statement::Assignment { name, value } => todo!(),
            Statement::Break => todo!(),
            Statement::Expression(expr) => todo!(),
            Statement::FunctionDeclaration { name, params, body } => todo!(),
            Statement::If {
                condition,
                body,
                else_body,
            } => todo!(),
            Statement::LetDeclaration { name, value } => todo!(),
            Statement::Loop { body } => todo!(),
            Statement::Nop => todo!(),
            Statement::Return(expr) => todo!(),
        }
    }
}
