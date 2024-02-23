use crate::tokenizer::tokens::Token;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Usage: isotope `file`")]
    Usage,
    #[error("File extension must be `.isotope` or `⚛️`")]
    FileExtension,
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error(transparent)]
    Tokenization(#[from] TokenizationError),
    #[error(transparent)]
    Parsing(#[from] ParsingError),
    #[error(transparent)]
    CodeGeneration(#[from] CodeGenerationError),
}

#[derive(thiserror::Error, Debug)]
pub enum TokenizationError {
    #[error("Invalid number")]
    InvalidNumber,
    #[error("Invalid identifier")]
    InvalidIdentifier,
    #[error("Unrecognized character: {0}")]
    UnrecognizedCharacter(char),
}

#[derive(thiserror::Error, Debug)]
pub enum ParsingError {
    #[error("Expected `{0}`")]
    ExpectedToken(String),
    #[error("Unexpected token: `{0:?}`")]
    UnexpectedToken(Token),
    #[error("Unable to parse statement")]
    Statement,
    #[error("Expected expression")]
    ExpectedExpression,
    #[error("Unable to parse literal: `{0:?}`")]
    Literal(String),
}

#[derive(thiserror::Error, Debug)]
pub enum CodeGenerationError {}
