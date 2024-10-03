use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
pub enum LexerError {
    #[error(transparent)]
    InvalidUtf8(#[from] std::str::Utf8Error),
    #[error("Unexpected token: `{found}`")]
    UnexpectedToken {
        found: String,

        #[label = "here"]
        span: SourceSpan,
    },
    #[error("Unclosed block comment")]
    UnclosedBlockComment {
        #[label = "opened here"]
        span: SourceSpan,
    },
    #[error("Unclosed string literal")]
    UnclosedString {
        #[label = "opened here"]
        span: SourceSpan,
    },
}
