use crate::tokenizer::tokens::Token;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Usage: isotope `file`")]
    UsageError,
    #[error("File extension must be `.isotope` or `⚛️`")]
    FileExtensionError,
    #[error(transparent)]
    IOError(#[from] std::io::Error),
    #[error(transparent)]
    TokenizationError(#[from] TokenizationError),
    #[error(transparent)]
    ParsingError(#[from] ParsingError),
    #[error(transparent)]
    CodeGenerationError(#[from] CodeGenerationError),
}

#[derive(thiserror::Error, Debug)]
pub enum TokenizationError {
    #[error("Invalid number")]
    InvalidNumber,
    #[error("Invalid identifier")]
    InvalidIdentifier,
}

#[derive(thiserror::Error, Debug)]
pub enum ParsingError {
    #[error("Expected `{0}`")]
    ExpectedToken(String),
    #[error("Unexpected token: `{0:?}`")]
    UnexpectedToken(Token),
    #[error("Unable to parse statement")]
    StatementError,
    #[error("Expected expression")]
    ExpectedExpression,
    #[error("Unable to parse literal: `{0:?}`")]
    InvalidLiteral(String),
}

#[derive(thiserror::Error, Debug)]
pub enum CodeGenerationError {}
