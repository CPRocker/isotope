use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum LexerError {
    UnexpectedEOF {
        #[source_code]
        src: String,
    },
    UnexpectedToken {
        #[source_code]
        src: String,

        #[label = "here"]
        span: SourceSpan,
    },
    UnclosedBlockComment {
        #[source_code]
        src: String,

        #[label = "here"]
        span: SourceSpan,
    },
    UnclosedString {
        #[source_code]
        src: String,

        #[label = "here"]
        span: SourceSpan,
    },
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnexpectedEOF { .. } => write!(f, "Unexpected EOF"),
            LexerError::UnexpectedToken { src, span } => {
                let token = &src[span.offset()..span.offset() + span.len()];
                write!(f, "Unexpected token: `{}`", token)
            }
            LexerError::UnclosedBlockComment { .. } => write!(f, "Unclosed block comment"),
            LexerError::UnclosedString { .. } => write!(f, "Unclosed string literal"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'de> {
    pub kind: TokenKind,
    pub orig: &'de str,
    pub offset: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // single character
    Equals,
    LeftParen,
    RightParen,
    Minus,
    Plus,
    Slash,
    Star,
    Dot,
    Semicolon,
    // keywords
    Let,
    Return,
    // terminals
    Identifier,
    String,
    Number(f64),
}

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
}

impl<'de> Lexer<'de> {
    pub fn new(file_contents: &'de str) -> Self {
        Self {
            whole: file_contents,
            rest: file_contents,
            byte: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.rest.chars().next()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Started {
    Identifier,
    Number,
    Slash,
    String,
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let before_trim = self.rest.len();
        self.rest = self.rest.trim_start();
        self.byte += before_trim - self.rest.len();

        if self.rest.is_empty() {
            return None;
        }

        let c = self.rest.chars().next().unwrap();
        let c_onwards = self.rest;
        let offset = self.byte;
        self.rest = &self.rest[c.len_utf8()..];
        self.byte += c.len_utf8();

        macro_rules! single_char_token {
            ($kind:ident) => {
                Some(Ok(Token {
                    kind: TokenKind::$kind,
                    orig: &self.whole[offset..self.byte],
                    offset,
                }))
            };
        }

        let token = match c {
            '(' => return single_char_token!(LeftParen),
            ')' => return single_char_token!(RightParen),
            '-' => return single_char_token!(Minus),
            '+' => return single_char_token!(Plus),
            '*' => return single_char_token!(Star),
            '=' => return single_char_token!(Equals),
            '.' => return single_char_token!(Dot),
            ';' => return single_char_token!(Semicolon),
            '/' => Started::Slash,
            'a'..='z' | 'A'..='Z' | '_' => Started::Identifier,
            '0'..='9' => Started::Number,
            '"' => Started::String,
            _ => {
                return Some(Err(LexerError::UnexpectedToken {
                    src: self.whole.to_string(),
                    span: offset.into(),
                }))
            }
        };

        match token {
            Started::Identifier => {
                let id = c_onwards
                    .split_once(|d| !matches!(d, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
                    .map_or(c_onwards, |(id, _)| id);

                self.rest = &c_onwards[id.len()..];
                self.byte = offset + id.len();

                macro_rules! ident {
                    ($kind:ident) => {
                        Some(Ok(Token {
                            kind: TokenKind::$kind,
                            orig: id,
                            offset,
                        }))
                    };
                }

                match id {
                    "let" => ident!(Let),
                    "return" => ident!(Return),
                    _ => ident!(Identifier),
                }
            }
            Started::Number => {
                let num = c_onwards
                    .split_once(|d| !matches!(d, '0'..='9' | '.'))
                    .map_or(c_onwards, |(num, _)| num);
                let parts: Vec<&str> = num.split('.').collect();

                let n = match parts.len() {
                    1 => {
                        // int
                        let num = parts[0];
                        num.parse::<u32>()
                            .expect("Must be an integer from match above")
                            .into()
                    }
                    2.. => {
                        // float
                        let num = parts[0..=1].join(".");
                        num.parse::<f64>()
                            .expect("Must be a float from match above")
                    }
                    _ => unreachable!("Must be a number from match above"),
                };

                self.rest = &c_onwards[num.len()..];
                self.byte = offset + num.len();

                Some(Ok(Token {
                    kind: TokenKind::Number(n),
                    orig: &self.whole[offset..self.byte],
                    offset,
                }))
            }
            Started::Slash => match self.peek() {
                Some('/') => {
                    // this is a single line comment
                    let slash = self.rest.chars().next().unwrap();
                    self.rest = &self.rest[slash.len_utf8()..];
                    self.byte += slash.len_utf8();

                    if let Some((comment, rest)) = self.rest.split_once('\n') {
                        self.rest = rest;
                        self.byte += comment.len() + '\n'.len_utf8();
                        return self.next();
                    }

                    None
                }
                Some('*') => {
                    /* this is a block comment */
                    let star = self.rest.chars().next().unwrap();
                    self.rest = &self.rest[star.len_utf8()..];
                    self.byte += star.len_utf8();

                    if let Some((comment, rest)) = self.rest.split_once("*/") {
                        self.rest = rest;
                        self.byte += comment.len() + "*/".len();
                        return self.next();
                    }

                    Some(Err(LexerError::UnclosedBlockComment {
                        src: self.whole.to_string(),
                        span: (offset..self.byte).into(),
                    }))
                }
                _ => single_char_token!(Slash),
            },
            Started::String => {
                if let Some((literal, rest)) = self.rest.split_once('"') {
                    self.rest = rest;
                    self.byte += literal.len() + '"'.len_utf8();
                    let offset = self.byte - literal.len() - 2 * '"'.len_utf8();
                    return Some(Ok(Token {
                        kind: TokenKind::String,
                        orig: &self.whole[offset..self.byte],
                        offset,
                    }));
                }

                Some(Err(LexerError::UnclosedString {
                    src: self.whole.to_string(),
                    span: (offset..self.byte).into(),
                }))
            }
        }
    }
}
