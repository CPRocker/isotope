use std::{fmt::Display, iter::Peekable};

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::lexer::{Lexer, LexerError, Token, TokenKind};

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
pub enum ParserError {
    #[error(transparent)]
    LexerError(#[from] LexerError),
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
    ExpectedIdentifier {
        #[source_code]
        src: String,
        #[label = "here"]
        span: SourceSpan,
    },
}

impl From<&LexerError> for ParserError {
    fn from(err: &LexerError) -> ParserError {
        ParserError::LexerError(err.clone())
    }
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::LexerError(_) => write!(f, "Lexer error"),
            ParserError::UnexpectedEOF { .. } => write!(f, "Unexpected EOF"),
            ParserError::UnexpectedToken { src, span } => {
                let token = &src[span.offset()..span.offset() + span.len()];
                write!(f, "Unexpected token: `{}`", token)
            }
            ParserError::ExpectedIdentifier { .. } => write!(f, "Expected identifier"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'de> {
    Nop,
    LetDeclaration(Identifier<'de>, Expr<'de>),
    Assignment(Identifier<'de>, Expr<'de>),
    Return(Expr<'de>),
    Expr(Expr<'de>),
}

impl<'de> Display for Statement<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Nop => write!(f, "nop;"),
            Statement::LetDeclaration(ident, expr) => write!(f, "let {} = {};", ident, expr),
            Statement::Assignment(ident, expr) => write!(f, "{} = {};", ident, expr),
            Statement::Return(expr) => write!(f, "return {};", expr),
            Statement::Expr(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier<'de> {
    name: &'de str,
}

impl<'de> Display for Identifier<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'de> {
    Atom(Atom<'de>),
    Cons(Op, Vec<Expr<'de>>),
}

impl<'de> Display for Expr<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Atom(Atom::Identifier(s)) => write!(f, "{}", s),
            Expr::Atom(Atom::Number(n)) => write!(f, "{}", n),
            Expr::Atom(Atom::String(s)) => write!(f, "\"{}\"", s),
            Expr::Cons(op, args) => {
                write!(f, "({}", op)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'de> {
    Identifier(&'de str),
    Number(f64),
    String(&'de str),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Assign,
    Field,
    Add,
    Sub,
    Mul,
    Div,
    Pos,
    Neg,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Assign => write!(f, "="),
            Op::Field => write!(f, "."),
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Pos => write!(f, "+"),
            Op::Neg => write!(f, "-"),
        }
    }
}

pub struct Parser<'de> {
    source: &'de str,
    lexer: Peekable<Lexer<'de>>,
}

impl<'de> Parser<'de> {
    pub fn new(file_contents: &'de str) -> Self {
        Self {
            source: file_contents,
            lexer: Lexer::new(file_contents).peekable(),
        }
    }
}

impl<'de> Iterator for Parser<'de> {
    type Item = Result<Statement<'de>, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.peek()?;
        Some(self.parse_statement())
    }
}

impl<'de> Parser<'de> {
    fn expect_token(&mut self, expected: TokenKind) -> Result<Token<'de>, ParserError> {
        match self.lexer.next() {
            Some(Ok(t @ Token { kind, orig, offset })) => {
                if kind == expected {
                    return Ok(t);
                }

                Err(ParserError::UnexpectedToken {
                    src: self.source.to_string(),
                    span: (offset..offset + orig.len()).into(),
                })
            }
            Some(Err(e)) => Err(e.into()),
            None => Err(ParserError::UnexpectedEOF {
                src: self.source.to_string(),
            }),
        }
    }

    fn parse_statement(&mut self) -> Result<Statement<'de>, ParserError> {
        match self.lexer.peek() {
            Some(Ok(Token {
                kind: TokenKind::Semicolon,
                ..
            })) => {
                self.lexer.next();
                Ok(Statement::Nop)
            }
            Some(Ok(Token {
                kind: TokenKind::Let,
                ..
            })) => {
                self.lexer.next();
                let ident = self.parse_identifier()?;
                self.expect_token(TokenKind::Eq)?;
                let expr = self.parse_expr()?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Statement::LetDeclaration(ident, expr))
            }
            Some(Ok(Token {
                kind: TokenKind::Identifier(_),
                ..
            })) => {
                let ident = self.parse_identifier()?;
                self.expect_token(TokenKind::Eq)?;
                let expr = self.parse_expr()?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Statement::Assignment(ident, expr))
            }
            Some(Ok(Token {
                kind: TokenKind::Return,
                ..
            })) => {
                self.lexer.next();
                let expr = self.parse_expr()?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Statement::Return(expr))
            }
            Some(Err(e)) => Err(e.into()),
            None => Err(ParserError::UnexpectedEOF {
                src: self.source.to_string(),
            }),
            Some(Ok(_)) => self.parse_expr().map(Statement::Expr),
        }
    }

    fn parse_identifier(&mut self) -> Result<Identifier<'de>, ParserError> {
        match self.lexer.next() {
            Some(Ok(Token {
                kind: TokenKind::Identifier(s),
                ..
            })) => Ok(Identifier { name: s }),
            Some(Ok(Token { orig, offset, .. })) => Err(ParserError::ExpectedIdentifier {
                src: self.source.to_string(),
                span: (offset..offset + orig.len()).into(),
            }),
            Some(Err(e)) => Err(e.into()),
            None => Err(ParserError::UnexpectedEOF {
                src: self.source.to_string(),
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr<'de>, ParserError> {
        if self.lexer.peek().is_none() {
            return Err(ParserError::UnexpectedEOF {
                src: self.source.to_string(),
            });
        }

        self.parse_expr_within(0)
    }

    fn parse_expr_within(&mut self, min_bp: u8) -> Result<Expr<'de>, ParserError> {
        let mut lhs = match self.lexer.next() {
            // literals
            Some(Ok(Token {
                kind: TokenKind::Number(n),
                ..
            })) => Expr::Atom(Atom::Number(n)),
            Some(Ok(Token {
                kind: TokenKind::String(s),
                ..
            })) => Expr::Atom(Atom::String(s)),
            Some(Ok(Token {
                kind: TokenKind::Identifier(s),
                ..
            })) => Expr::Atom(Atom::Identifier(s)),
            // parenthesized expression
            Some(Ok(Token {
                kind: TokenKind::LeftParen,
                ..
            })) => {
                let lhs = self.parse_expr_within(0)?;
                self.expect_token(TokenKind::RightParen)?;
                lhs
            }
            // prefix operators
            Some(Ok(Token {
                kind: TokenKind::Plus,
                ..
            })) => {
                let op = Op::Pos;
                let ((), r_bp) =
                    Self::prefix_binding_power(&op).expect("negate is a valid prefix op");
                let rhs = self.parse_expr_within(r_bp)?;
                Expr::Cons(op, vec![rhs])
            }
            Some(Ok(Token {
                kind: TokenKind::Minus,
                ..
            })) => {
                let op = Op::Neg;
                let ((), r_bp) =
                    Self::prefix_binding_power(&op).expect("negate is a valid prefix op");
                let rhs = self.parse_expr_within(r_bp)?;
                Expr::Cons(op, vec![rhs])
            }
            // errors
            Some(Err(e)) => return Err(e.into()),
            None => {
                return Err(ParserError::UnexpectedEOF {
                    src: self.source.to_string(),
                });
            }
            Some(Ok(Token { orig, offset, .. })) => {
                return Err(ParserError::UnexpectedToken {
                    src: self.source.to_string(),
                    span: (offset..offset + orig.len()).into(),
                });
            }
        };

        loop {
            let op = match self.lexer.peek() {
                Some(Ok(Token {
                    kind: TokenKind::Eq,
                    ..
                })) => Op::Assign,
                Some(Ok(Token {
                    kind: TokenKind::Plus,
                    ..
                })) => Op::Add,
                Some(Ok(Token {
                    kind: TokenKind::Minus,
                    ..
                })) => Op::Sub,
                Some(Ok(Token {
                    kind: TokenKind::Star,
                    ..
                })) => Op::Mul,
                Some(Ok(Token {
                    kind: TokenKind::Slash,
                    ..
                })) => Op::Div,
                Some(Ok(Token {
                    kind: TokenKind::Dot,
                    ..
                })) => Op::Field,
                Some(Err(e)) => return Err(e.into()),
                None => break,
                Some(Ok(Token { .. })) => break,
            };

            if let Some((l_bp, ())) = Self::postfix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                lhs = Expr::Cons(op, vec![lhs]);
                continue;
            }

            if let Some((l_bp, r_bp)) = Self::infix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                let rhs = self.parse_expr_within(r_bp)?;
                lhs = Expr::Cons(op, vec![lhs, rhs]);
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn prefix_binding_power(op: &Op) -> Option<((), u8)> {
        let res = match op {
            Op::Pos | Op::Neg => ((), 9),
            _ => return None,
        };
        Some(res)
    }

    fn infix_binding_power(op: &Op) -> Option<(u8, u8)> {
        let res = match op {
            Op::Assign => (2, 1),
            Op::Add | Op::Sub => (5, 6),
            Op::Mul | Op::Div => (7, 8),
            Op::Field => (14, 13),
            _ => return None,
        };
        Some(res)
    }

    fn postfix_binding_power(op: &Op) -> Option<(u8, ())> {
        let res = match op {
            _ => return None,
        };
        Some(res)
    }
}

#[cfg(test)]
mod parse {
    use super::*;

    #[test]
    fn eof() {
        let mut parser = Parser::new("");

        assert!(parser.next().is_none());
    }

    #[test]
    fn nop() {
        let mut parser = Parser::new(";;;");

        if let Some(Ok(expr)) = parser.next() {
            assert_eq!(expr, Statement::Nop);
        } else {
            panic!("Invalid expression");
        };
    }

    #[test]
    fn let_statement() {
        let mut parser = Parser::new("let x = 1;");

        if let Some(Ok(expr)) = parser.next() {
            assert_eq!(format!("{}", expr), "let x = 1;");
        } else {
            panic!("Invalid expression");
        };
    }

    #[test]
    fn assignment_statement() {
        let mut parser = Parser::new("x = 1;");

        if let Some(Ok(expr)) = parser.next() {
            assert_eq!(format!("{}", expr), "x = 1;");
        } else {
            panic!("Invalid expression");
        };
    }

    #[test]
    fn return_statement() {
        let mut parser = Parser::new("return 1;");

        if let Some(Ok(expr)) = parser.next() {
            assert_eq!(format!("{}", expr), "return 1;");
        } else {
            panic!("Invalid expression");
        };
    }

    mod expr {
        use super::*;

        #[test]
        fn prefix_op_precedence() {
            let mut parser = Parser::new("-1 + 2");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "(+ (- 1) 2)");
            } else {
                panic!("Invalid expression");
            };
        }

        #[test]
        fn infix_op_precedence() {
            let mut parser = Parser::new("0 + 1 * 2 - 3 / 4");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "(- (+ 0 (* 1 2)) (/ 3 4))");
            } else {
                panic!("Invalid expression");
            };
        }

        // TODO
        // #[test]
        // fn postfix_op_precedence() {
        //     let mut parser = Parser::new("1 + 2 - 3");

        //     if let Ok(expr) = parser.parse_expr() {
        //         assert_eq!(format!("{}", expr), "(+ 1 (- 2 3))");
        //     } else {
        //         panic!("Invalid expression");
        //     };
        // }
    }

    // TODO: test error cases
}
