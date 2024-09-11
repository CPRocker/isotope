use std::fmt::Display;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
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

        #[label = "opened here"]
        span: SourceSpan,
    },
    UnclosedString {
        #[source_code]
        src: String,

        #[label = "opened here"]
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

impl<'de> Token<'de> {
    pub fn span(&self) -> std::ops::Range<usize> {
        self.offset..self.offset + self.orig.len()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind<'de> {
    Bang,
    BangEq,
    Caret,
    Comma,
    Dot,
    Eq,
    EqEq,
    Greater,
    GreaterEq,
    Identifier(&'de str),
    LeftBracket,
    LeftCurly,
    LeftParen,
    Less,
    LessEq,
    Let,
    Minus,
    Number(f64),
    Percent,
    Plus,
    Return,
    RightBracket,
    RightCurly,
    RightParen,
    Semicolon,
    Slash,
    Star,
    String(&'de str),
}

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangEq => write!(f, "!="),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Eq => write!(f, "="),
            TokenKind::EqEq => write!(f, "=="),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEq => write!(f, ">="),
            TokenKind::Identifier(id) => write!(f, "{}", id),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::LeftCurly => write!(f, "{{"),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEq => write!(f, "<="),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Number(n) => write!(f, "{}", n),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::RightCurly => write!(f, "}}"),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::String(s) => write!(f, "\"{}\"", s), // TODO: escape strings
        }
    }
}

pub struct Lexer<'de> {
    src: &'de str,
    rest: &'de str,
    offset: usize,
}

impl<'de> Lexer<'de> {
    pub fn new(file_contents: &'de str) -> Self {
        Self {
            src: file_contents,
            rest: file_contents,
            offset: 0,
        }
    }

    fn trim_and_check(&mut self) -> bool {
        self.trim();
        self.peek().is_none()
    }

    fn trim(&mut self) {
        let before_trim = self.rest.len();
        self.rest = self.rest.trim_start();
        self.offset += before_trim - self.rest.len();
    }

    fn peek(&self) -> Option<char> {
        self.rest.chars().next()
    }

    fn consume(&mut self) -> Option<char> {
        self.trim();

        let c = self.rest.chars().next()?;
        self.rest = &self.rest[c.len_utf8()..];
        self.offset += c.len_utf8();

        Some(c)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Started {
    Bang,
    Eq,
    Greater,
    Identifier,
    Less,
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

        let c_onwards = self.rest;
        let c_at = self.offset;
        let c = self.consume().expect("Already checked for empty");

        macro_rules! single_char_token {
            ($kind:ident) => {
                Some(Ok(Token {
                    kind: TokenKind::$kind,
                    orig: &self.src[c_at..self.offset],
                    offset: c_at,
                }))
            };
        }

        let token = match c {
            '^' => return single_char_token!(Caret),
            ',' => return single_char_token!(Comma),
            '.' => return single_char_token!(Dot),
            '[' => return single_char_token!(LeftBracket),
            '{' => return single_char_token!(LeftCurly),
            '(' => return single_char_token!(LeftParen),
            '-' => return single_char_token!(Minus),
            '%' => return single_char_token!(Percent),
            '+' => return single_char_token!(Plus),
            ']' => return single_char_token!(RightBracket),
            '}' => return single_char_token!(RightCurly),
            ')' => return single_char_token!(RightParen),
            ';' => return single_char_token!(Semicolon),
            '*' => return single_char_token!(Star),
            '!' => Started::Bang,
            '=' => Started::Eq,
            '>' => Started::Greater,
            'a'..='z' | 'A'..='Z' | '_' => Started::Identifier,
            '<' => Started::Less,
            '0'..='9' => Started::Number,
            '/' => Started::Slash,
            '"' => Started::String,
            _ => {
                return Some(Err(LexerError::UnexpectedToken {
                    src: self.src.to_string(),
                    span: (c_at..self.offset).into(),
                }))
            }
        };

        macro_rules! with_eq_or {
            ($if:ident, $else:ident) => {
                if let Some('=') = self.peek() {
                    self.consume();
                    Some(Ok(Token {
                        kind: TokenKind::$if,
                        orig: &self.src[c_at..self.offset],
                        offset: c_at,
                    }))
                } else {
                    Some(Ok(Token {
                        kind: TokenKind::$else,
                        orig: &self.src[c_at..self.offset],
                        offset: c_at,
                    }))
                }
            };
        }

        match token {
            Started::Bang => with_eq_or!(BangEq, Bang),
            Started::Eq => with_eq_or!(EqEq, Eq),
            Started::Greater => with_eq_or!(GreaterEq, Greater),
            Started::Identifier => {
                let id = c_onwards
                    .split_once(|d| !matches!(d, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
                    .map_or(c_onwards, |(id, _)| id);

                self.rest = &c_onwards[id.len()..];
                self.offset = c_at + id.len();

                macro_rules! reserved {
                    ($kind:ident) => {
                        Some(Ok(Token {
                            kind: TokenKind::$kind,
                            orig: id,
                            offset: c_at,
                        }))
                    };
                }

                match id {
                    "let" => reserved!(Let),
                    "return" => reserved!(Return),
                    _ => Some(Ok(Token {
                        kind: TokenKind::Identifier(id),
                        orig: id,
                        offset: c_at,
                    })),
                }
            }
            Started::Less => with_eq_or!(LessEq, Less),
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
                self.offset = c_at + num.len();

                Some(Ok(Token {
                    kind: TokenKind::Number(n),
                    orig: &self.src[c_at..self.offset],
                    offset: c_at,
                }))
            }
            Started::Slash => match self.peek() {
                Some('/') => {
                    // this is a single line comment
                    self.consume();

                    if let Some((comment, rest)) = self.rest.split_once('\n') {
                        self.rest = rest;
                        self.offset += comment.len() + '\n'.len_utf8();
                        return self.next();
                    }

                    None
                }
                Some('*') => {
                    /* this is a block comment */
                    self.consume();

                    if let Some((comment, rest)) = self.rest.split_once("*/") {
                        self.rest = rest;
                        self.offset += comment.len() + "*/".len();
                        return self.next();
                    }

                    Some(Err(LexerError::UnclosedBlockComment {
                        src: self.src.to_string(),
                        span: (c_at..self.offset).into(),
                    }))
                }
                _ => single_char_token!(Slash),
            },
            Started::String => {
                if let Some((literal, rest)) = self.rest.split_once('"') {
                    self.rest = rest;
                    self.offset += literal.len() + '"'.len_utf8();
                    return Some(Ok(Token {
                        kind: TokenKind::String(literal),
                        orig: &self.src[c_at..self.offset],
                        offset: c_at,
                    }));
                }

                Some(Err(LexerError::UnclosedString {
                    src: self.src.to_string(),
                    span: (c_at..self.offset).into(),
                }))
            }
        }
    }
}

#[cfg(test)]
mod lex {
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
    fn eof() {
        let src = "";
        let mut lexer = Lexer::new(src);

        assert!(lexer.next().is_none());
    }

    #[test]
    fn whitespace() {
        let src = "  \t\n\r";
        let mut lexer = Lexer::new(src);

        assert!(lexer.next().is_none());
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
    fn parens_brackets_curlys() {
        let src = "( ) [ ] { }";
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, LeftParen, "(", 0);
        assert_token!(lexer, RightParen, ")", 2);
        assert_token!(lexer, LeftBracket, "[", 4);
        assert_token!(lexer, RightBracket, "]", 6);
        assert_token!(lexer, LeftCurly, "{", 8);
    }

    #[test]
    fn keywords() {
        let src = "let return";
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, Let, "let", 0);
        assert_token!(lexer, Return, "return", 4);
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
    fn operators() {
        let src = "+ - * / % ^";
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, Plus, "+", 0);
        assert_token!(lexer, Minus, "-", 2);
        assert_token!(lexer, Star, "*", 4);
        assert_token!(lexer, Slash, "/", 6);
        assert_token!(lexer, Percent, "%", 8);
        assert_token!(lexer, Caret, "^", 10);
    }

    #[test]
    fn comparison_operators() {
        let src = "= == === ! != !!= > >= >>= < <= <<=";
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, Eq, "=", 0);
        assert_token!(lexer, EqEq, "==", 2);
        assert_token!(lexer, EqEq, "==", 5);
        assert_token!(lexer, Eq, "=", 7);
        assert_token!(lexer, Bang, "!", 9);
        assert_token!(lexer, BangEq, "!=", 11);
        assert_token!(lexer, Bang, "!", 14);
        assert_token!(lexer, BangEq, "!=", 15);
        assert_token!(lexer, Greater, ">", 18);
        assert_token!(lexer, GreaterEq, ">=", 20);
        assert_token!(lexer, Greater, ">", 23);
        assert_token!(lexer, GreaterEq, ">=", 24);
        assert_token!(lexer, Less, "<", 27);
        assert_token!(lexer, LessEq, "<=", 29);
        assert_token!(lexer, Less, "<", 32);
        assert_token!(lexer, LessEq, "<=", 33);
    }

    // TODO: test error cases
}
