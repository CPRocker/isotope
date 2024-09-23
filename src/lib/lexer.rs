use std::fmt::Display;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
pub enum LexerError {
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
pub struct Token<'iso> {
    pub kind: TokenKind,
    pub orig: &'iso str,
    pub offset: usize,
}

impl Token<'_> {
    pub fn span(&self) -> std::ops::Range<usize> {
        self.offset..self.offset + self.orig.len()
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TokenKind::Bang
            | TokenKind::BangEqual
            | TokenKind::Break
            | TokenKind::Caret
            | TokenKind::Comma
            | TokenKind::Dot
            | TokenKind::Equal
            | TokenKind::EqualEqual
            | TokenKind::Else
            | TokenKind::False
            | TokenKind::Function
            | TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::If
            | TokenKind::LeftBracket
            | TokenKind::LeftCurly
            | TokenKind::LeftParen
            | TokenKind::Let
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Loop
            | TokenKind::Minus
            | TokenKind::Percent
            | TokenKind::Plus
            | TokenKind::Return
            | TokenKind::RightBracket
            | TokenKind::RightCurly
            | TokenKind::RightParen
            | TokenKind::Semicolon
            | TokenKind::Slash
            | TokenKind::Star
            | TokenKind::True => write!(f, "{}", self.kind),
            TokenKind::Identifier | TokenKind::Number | TokenKind::String => {
                write!(f, "{}", self.orig)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Bang,
    BangEqual,
    Break,
    Caret,
    Comma,
    Dot,
    Else,
    Equal,
    EqualEqual,
    False,
    Function,
    Greater,
    GreaterEqual,
    Identifier,
    If,
    LeftBracket,
    LeftCurly,
    LeftParen,
    Less,
    LessEqual,
    Let,
    Loop,
    Minus,
    Number,
    Percent,
    Plus,
    Return,
    RightBracket,
    RightCurly,
    RightParen,
    Semicolon,
    Slash,
    Star,
    String,
    True,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::Break => write!(f, "break"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::Else => write!(f, "else"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Function => write!(f, "fn"),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::If => write!(f, "if"),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::LeftCurly => write!(f, "{{"),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Loop => write!(f, "loop"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::RightCurly => write!(f, "}}"),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::String => write!(f, "string"),
            TokenKind::True => write!(f, "true"),
        }
    }
}

pub struct Lexer<'iso> {
    src: &'iso str,
    rest: &'iso str,
    offset: usize,
}

impl<'iso> Lexer<'iso> {
    pub fn new(file_contents: &'iso str) -> Self {
        Self {
            src: file_contents,
            rest: file_contents,
            offset: 0,
        }
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

impl<'iso> Iterator for Lexer<'iso> {
    type Item = Result<Token<'iso>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.trim();
        match self.peek() {
            Some(_) => {}
            None => return None,
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
                        orig: self.src[c_at..self.offset].into(),
                        offset: c_at,
                    }))
                } else {
                    Some(Ok(Token {
                        kind: TokenKind::$else,
                        orig: self.src[c_at..self.offset].into(),
                        offset: c_at,
                    }))
                }
            };
        }

        match token {
            Started::Bang => with_eq_or!(BangEqual, Bang),
            Started::Eq => with_eq_or!(EqualEqual, Equal),
            Started::Greater => with_eq_or!(GreaterEqual, Greater),
            Started::Identifier => {
                let id = c_onwards
                    .split_once(|d| !matches!(d, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
                    .map_or(c_onwards, |(id, _)| id);

                self.rest = &c_onwards[id.len()..];
                self.offset = c_at + id.len();

                macro_rules! identifier {
                    ($kind:ident) => {
                        Some(Ok(Token {
                            kind: TokenKind::$kind,
                            orig: id,
                            offset: c_at,
                        }))
                    };
                }

                match id {
                    "break" => identifier!(Break),
                    "else" => identifier!(Else),
                    "false" => identifier!(False),
                    "fn" => identifier!(Function),
                    "if" => identifier!(If),
                    "let" => identifier!(Let),
                    "loop" => identifier!(Loop),
                    "return" => identifier!(Return),
                    "true" => identifier!(True),
                    _ => identifier!(Identifier),
                }
            }
            Started::Less => with_eq_or!(LessEqual, Less),
            Started::Number => {
                let mut num = c_onwards
                    .split_once(|d| !matches!(d, '0'..='9' | '.'))
                    .map_or(c_onwards, |(num, _)| num);
                let parts: Vec<&str> = num.split('.').collect();

                match parts.len() {
                    1 => {
                        // int
                        let s = parts[0];
                        num = &c_onwards[..s.len()];
                    }
                    2.. => {
                        // float
                        let s = parts[0..=1].join(".");
                        num = &c_onwards[..s.len()];
                    }
                    _ => unreachable!("Must be a number from match above"),
                };

                self.rest = &c_onwards[num.len()..];
                self.offset = c_at + num.len();

                Some(Ok(Token {
                    kind: TokenKind::Number,
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
                        kind: TokenKind::String,
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
        let src = "let return fn if else loop break true false";
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, Let, "let", 0);
        assert_token!(lexer, Return, "return", 4);
        assert_token!(lexer, Function, "fn", 11);
        assert_token!(lexer, If, "if", 14);
        assert_token!(lexer, Else, "else", 17);
        assert_token!(lexer, Loop, "loop", 22);
        assert_token!(lexer, Break, "break", 27);
        assert_token!(lexer, True, "true", 33);
        assert_token!(lexer, False, "false", 38);
    }

    #[test]
    fn numbers() {
        let src = "123 123.456 123.456.789";
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, Number, "123", 0);
        assert_token!(lexer, Number, "123.456", 4);
        assert_token!(lexer, Number, "123.456", 12);
        assert_token!(lexer, Dot, ".", 19);
        assert_token!(lexer, Number, "789", 20);
    }

    #[test]
    fn identifiers() {
        let src = "foo bar baz";
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, Identifier, "foo", 0);
        assert_token!(lexer, Identifier, "bar", 4);
        assert_token!(lexer, Identifier, "baz", 8);
    }

    #[test]
    fn strings() {
        let src = r#""foo" "bar" "b
        a
        z""#;
        let mut lexer = Lexer::new(src);

        assert_token!(lexer, String, "\"foo\"", 0);
        assert_token!(lexer, String, "\"bar\"", 6);
        assert_token!(lexer, String, "\"b\n        a\n        z\"", 12);
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

        assert_token!(lexer, Equal, "=", 0);
        assert_token!(lexer, EqualEqual, "==", 2);
        assert_token!(lexer, EqualEqual, "==", 5);
        assert_token!(lexer, Equal, "=", 7);
        assert_token!(lexer, Bang, "!", 9);
        assert_token!(lexer, BangEqual, "!=", 11);
        assert_token!(lexer, Bang, "!", 14);
        assert_token!(lexer, BangEqual, "!=", 15);
        assert_token!(lexer, Greater, ">", 18);
        assert_token!(lexer, GreaterEqual, ">=", 20);
        assert_token!(lexer, Greater, ">", 23);
        assert_token!(lexer, GreaterEqual, ">=", 24);
        assert_token!(lexer, Less, "<", 27);
        assert_token!(lexer, LessEqual, "<=", 29);
        assert_token!(lexer, Less, "<", 32);
        assert_token!(lexer, LessEqual, "<=", 33);
    }

    mod errors {
        use super::*;
        #[test]
        fn unexpected_token() {
            let src = "let $ = 1;";
            let mut lexer = Lexer::new(src);

            assert_token!(lexer, Let, "let", 0);

            match lexer.next() {
                Some(Err(LexerError::UnexpectedToken { span, .. })) => {
                    assert_eq!(span, (4..5).into())
                }
                _ => panic!("Expected `LexerError::UnexpectedToken`"),
            }
        }

        #[test]
        fn unclosed_block_comment() {
            let src = "let /* testing  = 1;";
            let mut lexer = Lexer::new(src);

            assert_token!(lexer, Let, "let", 0);

            match lexer.next() {
                Some(Err(LexerError::UnclosedBlockComment { span, .. })) => {
                    assert_eq!(span, (4..6).into())
                }
                _ => panic!("Expected `LexerError::UnclosedBlockComment`"),
            }
        }

        #[test]
        fn unclosed_string() {
            let src = "let x = \"hello there;";
            let mut lexer = Lexer::new(src);

            assert_token!(lexer, Let, "let", 0);
            assert_token!(lexer, Identifier, "x", 4);
            assert_token!(lexer, Equal, "=", 6);

            match lexer.next() {
                Some(Err(LexerError::UnclosedString { span, .. })) => {
                    assert_eq!(span, (8..9).into())
                }
                _ => panic!("Expected `LexerError::UnclosedString`"),
            }
        }
    }
}
