use miette::Result;

mod error;
mod reader;
mod token;

pub use error::LexerError;
pub use token::Token;
pub use token::TokenKind;

use crate::source::Source;

pub struct Lexer<'iso> {
    reader: reader::CharReader<'iso>,
}

impl<'iso> Lexer<'iso> {
    pub fn new(src: &'iso Source) -> Result<Self, LexerError> {
        Ok(Self {
            reader: reader::CharReader::new(src).map_err(LexerError::from)?,
        })
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

impl<'iso> Iterator for Lexer<'iso> {
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
    use std::str::FromStr;

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
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut lexer = Lexer::new(&src).expect("failed to create lexer");

        assert!(lexer.next().is_none());
    }

    #[test]
    fn whitespace() {
        let code = "  \t\n\r";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut lexer = Lexer::new(&src).expect("failed to create lexer");

        assert!(lexer.next().is_none());
    }

    #[test]
    fn single_line_comments() {
        let code = r#"
        // this is a single line comment
        // this is a second line comment
        ;
        "#;
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut lexer = Lexer::new(&src).expect("failed to create lexer");

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
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut lexer = Lexer::new(&src).expect("failed to create lexer");

        assert_token!(lexer, Semicolon, ";", 67);
    }

    #[test]
    fn parens_brackets_curlys() {
        let code = "( ) [ ] { }";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut lexer = Lexer::new(&src).expect("failed to create lexer");

        assert_token!(lexer, LeftParen, "(", 0);
        assert_token!(lexer, RightParen, ")", 2);
        assert_token!(lexer, LeftBracket, "[", 4);
        assert_token!(lexer, RightBracket, "]", 6);
        assert_token!(lexer, LeftCurly, "{", 8);
    }

    #[test]
    fn keywords() {
        let code = "let return fn if else loop break true false";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut lexer = Lexer::new(&src).expect("failed to create lexer");

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
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut lexer = Lexer::new(&src).expect("failed to create lexer");

        assert_token!(lexer, Number, "123", 0);
        assert_token!(lexer, Number, "123.456", 4);
        assert_token!(lexer, Number, "123.456", 12);
        assert_token!(lexer, Dot, ".", 19);
        assert_token!(lexer, Number, "789", 20);
    }

    #[test]
    fn identifiers() {
        let code = "foo bar baz";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut lexer = Lexer::new(&src).expect("failed to create lexer");

        assert_token!(lexer, Identifier, "foo", 0);
        assert_token!(lexer, Identifier, "bar", 4);
        assert_token!(lexer, Identifier, "baz", 8);
    }

    #[test]
    fn strings() {
        let code = r#""foo" "bar" "b
        a
        z""#;
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut lexer = Lexer::new(&src).expect("failed to create lexer");

        assert_token!(lexer, String, "\"foo\"", 0);
        assert_token!(lexer, String, "\"bar\"", 6);
        assert_token!(lexer, String, "\"b\n        a\n        z\"", 12);
    }

    #[test]
    fn operators() {
        let code = "+ - * / % ^";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut lexer = Lexer::new(&src).expect("failed to create lexer");

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
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut lexer = Lexer::new(&src).expect("failed to create lexer");

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
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut lexer = Lexer::new(&src).expect("failed to create lexer");

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
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut lexer = Lexer::new(&src).expect("failed to create lexer");

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
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut lexer = Lexer::new(&src).expect("failed to create lexer");

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
