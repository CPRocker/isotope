use std::fmt::Display;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug, Clone)]
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
    pub kind: TokenKind<'de>,
    pub orig: &'de str,
    pub offset: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind<'de> {
    // single character
    Eq,
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
    Identifier(&'de str),
    String(&'de str),
    Number(f64),
}

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Eq => write!(f, "="),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Identifier(id) => write!(f, "{}", id),
            TokenKind::String(s) => write!(f, "\"{}\"", s), // TODO: escape strings
            TokenKind::Number(n) => write!(f, "{}", n),
        }
    }
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

    pub fn offset(&self) -> usize {
        self.byte
    }

    fn trim_and_check(&mut self) -> bool {
        self.trim();
        self.peek().is_none()
    }

    fn trim(&mut self) {
        let before_trim = self.rest.len();
        self.rest = self.rest.trim_start();
        self.byte += before_trim - self.rest.len();
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
        if self.trim_and_check() {
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
            '=' => return single_char_token!(Eq),
            '.' => return single_char_token!(Dot),
            ';' => return single_char_token!(Semicolon),
            '/' => Started::Slash,
            'a'..='z' | 'A'..='Z' | '_' => Started::Identifier,
            '0'..='9' => Started::Number,
            '"' => Started::String,
            _ => {
                return Some(Err(LexerError::UnexpectedToken {
                    src: self.whole.to_string(),
                    span: (offset..self.byte).into(),
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

                macro_rules! reserved {
                    ($kind:ident) => {
                        Some(Ok(Token {
                            kind: TokenKind::$kind,
                            orig: id,
                            offset,
                        }))
                    };
                }

                match id {
                    "let" => reserved!(Let),
                    "return" => reserved!(Return),
                    _ => Some(Ok(Token {
                        kind: TokenKind::Identifier(id),
                        orig: id,
                        offset,
                    })),
                }
            }
            Started::Number => {
                let mut num = c_onwards
                    .split_once(|d| !matches!(d, '0'..='9' | '.'))
                    .map_or(c_onwards, |(num, _)| num);
                let parts: Vec<&str> = num.split('.').collect();

                let n = match parts.len() {
                    1 => {
                        // int
                        let s = parts[0];
                        num = &c_onwards[..s.len()];
                        num.parse::<u32>()
                            .expect("Must be an integer from match above")
                            .into()
                    }
                    2.. => {
                        // float
                        let s = parts[0..=1].join(".");
                        num = &c_onwards[..s.len()];
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
                        kind: TokenKind::String(literal),
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

#[cfg(test)]
mod tests {
    use super::TokenKind::*;
    use super::*;

    macro_rules! assert_token {
        ($lexer:ident, $kind:ident, $orig:literal, $offset:literal) => {
            if let Some(Ok(Token {
                kind: TokenKind::$kind,
                orig,
                offset,
            })) = $lexer.next()
            {
                assert_eq!(orig, $orig);
                assert_eq!(offset, $offset);
            } else {
                panic!("Expected a {} token", $kind);
            }
        };
        ($lexer:ident, $kind:ident, $orig:literal, $offset:literal, $value:literal) => {
            if let Some(Ok(Token {
                kind: TokenKind::$kind(val),
                orig,
                offset,
            })) = $lexer.next()
            {
                assert_eq!(val, $value);
                assert_eq!(orig, $orig);
                assert_eq!(offset, $offset);
            } else {
                panic!("Expected a {} token", $kind($value));
            }
        };
    }

    #[test]
    fn single_line_comments() {
        let src = r#"
        // this is a single line comment
        // this is a second line comment
        ;
        "#;
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, Semicolon, ";", 91);
    }

    #[test]
    fn block_comments() {
        let src = r#"
        /*
            this is a block comment
        */
        ;
        "#;
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, Semicolon, ";", 67);
    }

    #[test]
    fn numbers() {
        let src = "123 123.456 123.456.789";
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, Number, "123", 0, 123.);
        assert_token!(lexer, Number, "123.456", 4, 123.456);
        assert_token!(lexer, Number, "123.456", 12, 123.456);
        assert_token!(lexer, Dot, ".", 19);
        assert_token!(lexer, Number, "789", 20, 789.);
    }

    #[test]
    fn identifiers() {
        let src = "foo bar baz";
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, Identifier, "foo", 0, "foo");
        assert_token!(lexer, Identifier, "bar", 4, "bar");
        assert_token!(lexer, Identifier, "baz", 8, "baz");
    }

    #[test]
    fn strings() {
        let src = r#""foo" "bar" "b
        a
        z""#;
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, String, "\"foo\"", 0, "foo");
        assert_token!(lexer, String, "\"bar\"", 6, "bar");
        assert_token!(
            lexer,
            String,
            "\"b\n        a\n        z\"",
            12,
            "b\n        a\n        z"
        );
    }

    #[test]
    fn whitespace() {
        let src = "  \t\n\r";
        let mut lexer = Lexer::new(src);

        assert!(lexer.next().is_none());
    }

    #[test]
    fn eof() {
        let src = "";
        let mut lexer = Lexer::new(src);

        assert!(lexer.next().is_none());
    }

    // TODO: test errors
}
