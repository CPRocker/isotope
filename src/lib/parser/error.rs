use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::lexer::LexerError;

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
pub enum ParserError {
    #[error(transparent)]
    LexerError(
        #[from]
        #[diagnostic_source]
        LexerError,
    ),
    #[error("Failed to parse float: {error}")]
    ParseFloatError {
        #[label = "here"]
        span: SourceSpan,

        #[source]
        error: std::num::ParseFloatError,
    },
    #[error("Unexpected end of file")]
    UnexpectedEOF,
    #[error("Expected {expected}, found {found}")]
    ExpectedToken {
        expected: String,

        found: String,

        #[label = "here"]
        span: SourceSpan,
    },
    #[error("Expected an expression")]
    ExpectedExpression {
        #[label = "here"]
        span: SourceSpan,
    },
    #[error("Expected an identifier")]
    ExpectedIdentifier {
        #[label = "here"]
        span: SourceSpan,
    },
    #[error("Invalid else statement: expected `else if` or `else` block")]
    InvalidElseStatement {
        #[label = "here"]
        span: SourceSpan,
    },
    #[error("Invalid postfix operator: {op}")]
    InvalidPostfixOperator {
        op: String,

        #[label = "here"]
        span: SourceSpan,
    },
    #[error("Unclosed parameter list")]
    #[diagnostic(help("Did you forget to add a right parenthesis?"))]
    UnclosedParamList {
        #[label = "here"]
        span: SourceSpan,
    },
    #[error("Unclosed block")]
    UnclosedBlock {
        #[label = "here"]
        span: SourceSpan,
    },
}

impl From<&LexerError> for ParserError {
    fn from(err: &LexerError) -> ParserError {
        ParserError::LexerError(err.to_owned())
    }
}
