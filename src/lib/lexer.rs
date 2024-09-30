use std::fmt::Display;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

mod reader;

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
pub enum LexerError {
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

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub orig: String,
    pub offset: usize,
}

impl Token {
    pub fn span(&self) -> std::ops::Range<usize> {
        self.offset..self.offset + self.orig.len()
    }
}

impl Display for Token {
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

pub struct Lexer<'iso, R>
where
    R: std::io::BufRead,
{
    reader: reader::CharReader<'iso, R>,
}

impl<'iso, R> Lexer<'iso, R>
where
    R: std::io::BufRead,
{
    pub fn new(src: &'iso mut R) -> Self {
        Self {
            reader: reader::CharReader::new(src),
        }
    }

    fn skip_read_while(&mut self, predicate: impl Fn(&char) -> bool) {
        self.reader.skip_read_while(predicate)
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

impl<'iso, R> Iterator for Lexer<'iso, R>
where
    R: std::io::BufRead,
{
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_read_while(|c| c.is_whitespace());

        let offset = self.reader.offset();
        let c = self.reader.next()?;
        let mut orig = c.to_string();

        macro_rules! single_char_token {
            ($kind:ident) => {
                Some(Ok(Token {
                    kind: TokenKind::$kind,
                    orig,
                    offset,
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
                    found: c.to_string(),
                    span: (offset..offset + orig.len()).into(),
                }))
            }
        };

        macro_rules! with_eq_or {
            ($if:ident, $else:ident) => {
                if let Some('=') = self.reader.peek() {
                    orig.push(self.reader.next().expect("already peeked"));
                    Some(Ok(Token {
                        kind: TokenKind::$if,
                        orig,
                        offset,
                    }))
                } else {
                    Some(Ok(Token {
                        kind: TokenKind::$else,
                        orig,
                        offset,
                    }))
                }
            };
        }

        match token {
            Started::Bang => with_eq_or!(BangEqual, Bang),
            Started::Eq => with_eq_or!(EqualEqual, Equal),
            Started::Greater => with_eq_or!(GreaterEqual, Greater),
            Started::Identifier => {
                while let Some(c) = self.reader.peek() {
                    if matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9') {
                        orig.push(self.reader.next().expect("already peeked"));
                    } else {
                        break;
                    }
                }

                macro_rules! identifier {
                    ($kind:ident) => {
                        Some(Ok(Token {
                            kind: TokenKind::$kind,
                            orig,
                            offset,
                        }))
                    };
                }

                match orig.as_str() {
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
                let mut first_decimal = true;
                while let Some(c) = self.reader.peek() {
                    match c {
                        '0'..='9' => {
                            orig.push(self.reader.next().expect("already peeked"));
                        }
                        '.' if first_decimal => {
                            orig.push(self.reader.next().expect("already peeked"));
                            first_decimal = false;
                        }
                        '.' => break,
                        _ => break,
                    }
                }

                Some(Ok(Token {
                    kind: TokenKind::Number,
                    orig,
                    offset,
                }))
            }
            Started::Slash => match self.reader.peek() {
                Some('/') => {
                    // this is a single line comment
                    self.reader.next();

                    self.skip_read_while(|&c| c != '\n');
                    self.reader.next();

                    self.next()
                }
                Some('*') => {
                    /* this is a block comment */
                    orig.push(self.reader.next().expect("already peeked"));

                    loop {
                        self.skip_read_while(|&c| c != '*');
                        self.reader.next();

                        match self.reader.peek() {
                            Some('/') => {
                                self.reader.next();
                                return self.next();
                            }
                            None => {
                                return Some(Err(LexerError::UnclosedBlockComment {
                                    span: (offset..offset + orig.len()).into(),
                                }));
                            }
                            _ => continue,
                        }
                    }
                }
                _ => single_char_token!(Slash),
            },
            Started::String => {
                loop {
                    match self.reader.peek() {
                        Some(_) => {
                            let c = self.reader.next().expect("already peeked");
                            orig.push(c);

                            if c == '"' {
                                break;
                            }
                        }
                        None => {
                            return Some(Err(LexerError::UnclosedString {
                                span: (offset..offset + orig.len()).into(),
                            }));
                        }
                    }
                }

                Some(Ok(Token {
                    kind: TokenKind::String,
                    orig,
                    offset,
                }))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::TokenKind::*;
    use super::*;

    macro_rules! assert_token {
        ($lexer:ident, $kind:ident, $orig:literal, $offset:literal) => {
            match $lexer.next() {
                Some(Ok(Token {
                    kind: TokenKind::$kind,
                    orig,
                    offset,
                })) => {
                    assert_eq!(
                        orig, $orig,
                        "{} at {}, expected orig: {}",
                        $kind, offset, $orig
                    );
                    assert_eq!(
                        offset, $offset,
                        "{} at {}, expected offset: {}",
                        $kind, offset, $offset
                    );
                }
                Some(Ok(token)) => {
                    panic!("Expected {} at {}, got {:?}", $kind, $offset, token)
                }
                Some(Err(e)) => panic!("Expected {} at {}, got error: {}", $kind, $offset, e),
                None => panic!("Expected {} at {}", $kind, $offset),
            }
        };
    }

    #[test]
    fn eof() {
        let code = "";
        let mut src = Cursor::new(code);
        let mut lexer = Lexer::new(&mut src);

        assert!(lexer.next().is_none());
    }

    #[test]
    fn whitespace() {
        let code = "  \t\n\r";
        let mut src = Cursor::new(code);
        let mut lexer = Lexer::new(&mut src);

        assert!(lexer.next().is_none());
    }

    #[test]
    fn single_line_comments() {
        let code = r#"
        // this is a single line comment
        // this is a second line comment
        ;
        "#;
        let mut src = Cursor::new(code);
        let mut lexer = Lexer::new(&mut src);

        assert_token!(lexer, Semicolon, ";", 91);
    }

    #[test]
    fn block_comments() {
        let code = r#"
        /*
            this is a block comment
        */
        ;
        "#;
        let mut src = Cursor::new(code);
        let mut lexer = Lexer::new(&mut src);

        assert_token!(lexer, Semicolon, ";", 67);
    }

    #[test]
    fn parens_brackets_curlys() {
        let code = "( ) [ ] { }";
        let mut src = Cursor::new(code);
        let mut lexer = Lexer::new(&mut src);

        assert_token!(lexer, LeftParen, "(", 0);
        assert_token!(lexer, RightParen, ")", 2);
        assert_token!(lexer, LeftBracket, "[", 4);
        assert_token!(lexer, RightBracket, "]", 6);
        assert_token!(lexer, LeftCurly, "{", 8);
    }

    #[test]
    fn keywords() {
        let code = "let return fn if else loop break true false";
        let mut src = Cursor::new(code);
        let mut lexer = Lexer::new(&mut src);

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
        let code = "123 123.456 123.456.789";
        let mut src = Cursor::new(code);
        let mut lexer = Lexer::new(&mut src);

        assert_token!(lexer, Number, "123", 0);
        assert_token!(lexer, Number, "123.456", 4);
        assert_token!(lexer, Number, "123.456", 12);
        assert_token!(lexer, Dot, ".", 19);
        assert_token!(lexer, Number, "789", 20);
    }

    #[test]
    fn identifiers() {
        let code = "foo bar baz";
        let mut src = Cursor::new(code);
        let mut lexer = Lexer::new(&mut src);

        assert_token!(lexer, Identifier, "foo", 0);
        assert_token!(lexer, Identifier, "bar", 4);
        assert_token!(lexer, Identifier, "baz", 8);
    }

    #[test]
    fn strings() {
        let code = r#""foo" "bar" "b
        a
        z""#;
        let mut src = Cursor::new(code);
        let mut lexer = Lexer::new(&mut src);

        assert_token!(lexer, String, "\"foo\"", 0);
        assert_token!(lexer, String, "\"bar\"", 6);
        assert_token!(lexer, String, "\"b\n        a\n        z\"", 12);
    }

    #[test]
    fn operators() {
        let code = "+ - * / % ^";
        let mut src = Cursor::new(code);
        let mut lexer = Lexer::new(&mut src);

        assert_token!(lexer, Plus, "+", 0);
        assert_token!(lexer, Minus, "-", 2);
        assert_token!(lexer, Star, "*", 4);
        assert_token!(lexer, Slash, "/", 6);
        assert_token!(lexer, Percent, "%", 8);
        assert_token!(lexer, Caret, "^", 10);
    }

    #[test]
    fn comparison_operators() {
        let code = "= == === ! != !!= > >= >>= < <= <<=";
        let mut src = Cursor::new(code);
        let mut lexer = Lexer::new(&mut src);

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
            let code = "let $ = 1;";
            let mut src = Cursor::new(code);
            let mut lexer = Lexer::new(&mut src);

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
            let code = "let /* testing  = 1;";
            let mut src = Cursor::new(code);
            let mut lexer = Lexer::new(&mut src);

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
            let code = "let x = \"hello there;";
            let mut src = Cursor::new(code);
            let mut lexer = Lexer::new(&mut src);

            assert_token!(lexer, Let, "let", 0);
            assert_token!(lexer, Identifier, "x", 4);
            assert_token!(lexer, Equal, "=", 6);

            match lexer.next() {
                Some(Err(LexerError::UnclosedString { span, .. })) => {
                    assert_eq!(span, (8..21).into())
                }
                _ => panic!("Expected `LexerError::UnclosedString`"),
            }
        }
    }
}
