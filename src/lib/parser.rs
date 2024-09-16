use std::{fmt::Display, iter::Peekable};

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::lexer::{Lexer, LexerError, Token, TokenKind};

#[derive(Error, Diagnostic, Debug, Clone, PartialEq)]
pub enum ParserError {
    #[error(transparent)]
    LexerError(
        #[from]
        #[diagnostic_source]
        LexerError,
    ),
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
    ExpectedToken {
        #[source_code]
        src: String,
        #[label = "here"]
        span: SourceSpan,
        expected: String,
    },
    ExpectedExpression {
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
    InvalidPostfixOperator {
        #[source_code]
        src: String,
        #[label = "here"]
        span: SourceSpan,
    },
    #[diagnostic(help("Did you forget to add a right parenthesis?"))]
    UnclosedParamList {
        #[source_code]
        src: String,
        #[label = "here"]
        span: SourceSpan,
    },
    UnclosedBlock {
        #[source_code]
        src: String,
        #[label = "here"]
        span: SourceSpan,
    },
}

impl From<&LexerError> for ParserError {
    fn from(err: &LexerError) -> ParserError {
        ParserError::LexerError(err.to_owned())
    }
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::LexerError { .. } => write!(f, "Lexer error"),
            ParserError::UnexpectedEOF { .. } => write!(f, "Unexpected EOF"),
            ParserError::UnexpectedToken { src, span } => {
                let token = &src[span.offset()..span.offset() + span.len()];
                write!(f, "Unexpected token: `{}`", token)
            }
            ParserError::ExpectedToken {
                src,
                span,
                expected,
            } => {
                let found = &src[span.offset()..span.offset() + span.len()];
                write!(f, "Expected `{}` but found `{}`", expected, found)
            }
            ParserError::ExpectedExpression { .. } => write!(f, "Expected expression"),
            ParserError::ExpectedIdentifier { .. } => write!(f, "Expected identifier"),
            ParserError::InvalidPostfixOperator { src, span } => {
                let token = &src[span.offset()..span.offset() + span.len()];
                write!(f, "Invalid postfix operator: `{}`", token)
            }
            ParserError::UnclosedParamList { .. } => write!(f, "Unclosed parameter list"),
            ParserError::UnclosedBlock { .. } => write!(f, "Unclosed block"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'de> {
    Assignment {
        name: Identifier<'de>,
        value: Expr<'de>,
    },
    Expr(Expr<'de>),
    FunctionDeclaration {
        name: Identifier<'de>,
        params: Vec<Identifier<'de>>,
        body: Vec<Statement<'de>>,
    },
    LetDeclaration {
        name: Identifier<'de>,
        value: Expr<'de>,
    },
    Nop,
    Return(Expr<'de>),
}

impl<'de> Display for Statement<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Assignment { name, value } => write!(f, "{} = {};", name, value),
            Statement::Expr(expr) => write!(f, "{}", expr),
            Statement::FunctionDeclaration { name, params, body } => {
                write!(f, "fun {}(", name)?;
                for (i, param) in params.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") {{ ")?;
                for statement in body {
                    write!(f, "{} ", statement)?;
                }
                write!(f, "}}")
            }
            Statement::LetDeclaration { name, value } => write!(f, "let {} = {};", name, value),
            Statement::Nop => write!(f, "nop;"),
            Statement::Return(expr) => write!(f, "ret {};", expr),
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
    Add,
    Assign,
    Call,
    Div,
    Eq,
    Field,
    Gt,
    Gte,
    Index,
    Lt,
    Lte,
    Mod,
    Mul,
    Neg,
    Neq,
    Not,
    Pos,
    Pow,
    Sub,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Assign => write!(f, "="),
            Op::Call => write!(f, "()"),
            Op::Div => write!(f, "/"),
            Op::Eq => write!(f, "=="),
            Op::Field => write!(f, "."),
            Op::Gt => write!(f, ">"),
            Op::Gte => write!(f, ">="),
            Op::Index => write!(f, "[]"),
            Op::Lt => write!(f, "<"),
            Op::Lte => write!(f, "<="),
            Op::Mod => write!(f, "%"),
            Op::Mul => write!(f, "*"),
            Op::Neg => write!(f, "-"),
            Op::Neq => write!(f, "!="),
            Op::Not => write!(f, "!"),
            Op::Pos => write!(f, "+"),
            Op::Pow => write!(f, "^"),
            Op::Sub => write!(f, "-"),
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

                Err(ParserError::ExpectedToken {
                    src: self.source.to_string(),
                    span: (offset..offset + orig.len()).into(),
                    expected: format!("{}", expected),
                })
            }
            Some(Err(e)) => Err(e.into()),
            None => Err(ParserError::UnexpectedEOF {
                src: self.source.to_string(),
            }),
        }
    }

    fn parse_block(&mut self) -> Result<Vec<Statement<'de>>, ParserError> {
        let left_curly = self.expect_token(TokenKind::LeftCurly)?;
        let mut statements = vec![];

        loop {
            match self.lexer.peek() {
                Some(Ok(Token {
                    kind: TokenKind::RightCurly,
                    ..
                })) => {
                    break;
                }
                Some(Err(e)) => return Err(e.into()),
                None => {
                    return Err(ParserError::UnclosedBlock {
                        src: self.source.to_string(),
                        span: left_curly.span().into(),
                    });
                }
                Some(_) => statements.push(self.parse_statement()?),
            }
        }
        self.expect_token(TokenKind::RightCurly)
            .map_err(|_| ParserError::UnclosedBlock {
                src: self.source.to_string(),
                span: (left_curly.span()).into(),
            })?;

        Ok(statements)
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
                let name = self.parse_identifier()?;
                self.expect_token(TokenKind::Eq)?;
                let value = self.parse_expr()?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Statement::LetDeclaration { name, value })
            }
            Some(Ok(Token {
                kind: TokenKind::Fun,
                ..
            })) => {
                self.lexer.next();
                let name = self.parse_identifier()?;
                let params = self.parse_param_list()?;
                let body = self.parse_block()?;
                Ok(Statement::FunctionDeclaration { name, params, body })
            }
            Some(Ok(Token {
                kind: TokenKind::Identifier(_),
                ..
            })) => {
                let name = self.parse_identifier()?;
                self.expect_token(TokenKind::Eq)?;
                let value = self.parse_expr()?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Statement::Assignment { name, value })
            }
            Some(Ok(Token {
                kind: TokenKind::Ret,
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

    fn parse_param_list(&mut self) -> Result<Vec<Identifier<'de>>, ParserError> {
        let left_paren = self.expect_token(TokenKind::LeftParen)?;
        let mut last = left_paren.clone();

        let mut params = vec![];

        loop {
            let token = match self.lexer.peek() {
                Some(Ok(token)) => token.clone(),
                Some(Err(e)) => return Err(e.into()),
                None => {
                    return Err(ParserError::UnclosedParamList {
                        src: self.source.to_string(),
                        span: (left_paren.offset..last.span().end).into(),
                    })
                }
            };

            match token {
                Token {
                    kind: TokenKind::RightParen,
                    ..
                } => break,
                Token {
                    kind: TokenKind::Identifier(name),
                    ..
                } => {
                    last = self
                        .lexer
                        .next()
                        .expect("Checked on peek")
                        .expect("Checked on peek");
                    params.push(Identifier { name });

                    match self.lexer.peek() {
                        Some(Ok(Token {
                            kind: TokenKind::Comma,
                            ..
                        })) => {
                            last = self
                                .lexer
                                .next()
                                .expect("Checked on peek")
                                .expect("Checked on peek");
                        }
                        Some(Ok(Token {
                            kind: TokenKind::RightParen,
                            ..
                        })) => break,
                        Some(Err(e)) => return Err(e.into()),
                        _ => {
                            return Err(ParserError::UnclosedParamList {
                                src: self.source.to_string(),
                                span: (left_paren.offset..last.span().end).into(),
                            })
                        }
                    }
                }
                _ => {
                    return Err(ParserError::UnclosedParamList {
                        src: self.source.to_string(),
                        span: (left_paren.offset..last.span().end).into(),
                    });
                }
            }
        }
        self.expect_token(TokenKind::RightParen)
            .map_err(|_| ParserError::UnclosedParamList {
                src: self.source.to_string(),
                span: (left_paren.offset..last.span().end).into(),
            })?;

        Ok(params)
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

    /// Parses a list of expressions separated by the given separator token until the
    /// given end token.
    ///
    /// Note: this will not consume the end token
    fn parse_expr_list(
        &mut self,
        sep: TokenKind,
        end: TokenKind,
    ) -> Result<Vec<Expr<'de>>, ParserError> {
        let mut params: Vec<Expr<'de>> = vec![];

        loop {
            match self.lexer.peek() {
                Some(Ok(Token { kind, .. })) if kind == &end => break,
                Some(Ok(Token { kind, .. })) if kind == &sep => {
                    self.lexer.next();
                }
                _ => {
                    let param = self.parse_expr()?;
                    params.push(param);
                }
            }
        }

        Ok(params)
    }

    fn parse_expr(&mut self) -> Result<Expr<'de>, ParserError> {
        if self.lexer.peek().is_none() {
            return Err(ParserError::UnexpectedEOF {
                src: self.source.to_string(),
            });
        }

        self.parse_expr_within(0) // TODO: wrap with ExpectedExpression
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
                kind: TokenKind::Minus,
                ..
            })) => {
                let op = Op::Neg;
                let ((), r_bp) =
                    Self::prefix_binding_power(&op).expect("negate is a valid prefix op");
                let rhs = self.parse_expr_within(r_bp)?;
                Expr::Cons(op, vec![rhs])
            }
            Some(Ok(Token {
                kind: TokenKind::Bang,
                ..
            })) => {
                let op = Op::Not;
                let ((), r_bp) = Self::prefix_binding_power(&op).expect("not is a valid prefix op");
                let rhs = self.parse_expr_within(r_bp)?;
                Expr::Cons(op, vec![rhs])
            }
            Some(Ok(Token {
                kind: TokenKind::Plus,
                ..
            })) => {
                let op = Op::Pos;
                let ((), r_bp) =
                    Self::prefix_binding_power(&op).expect("postive is a valid prefix op");
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

        macro_rules! some_ok_token {
            ($kind:ident) => {
                Some(Ok(Token {
                    kind: TokenKind::$kind,
                    ..
                }))
            };
        }

        loop {
            let op = match self.lexer.peek() {
                some_ok_token!(Plus) => Op::Add,
                some_ok_token!(Eq) => Op::Assign,
                some_ok_token!(LeftParen) => Op::Call,
                some_ok_token!(Slash) => Op::Div,
                some_ok_token!(EqEq) => Op::Eq,
                some_ok_token!(Dot) => Op::Field,
                some_ok_token!(Greater) => Op::Gt,
                some_ok_token!(GreaterEq) => Op::Gte,
                some_ok_token!(LeftBracket) => Op::Index,
                some_ok_token!(Less) => Op::Lt,
                some_ok_token!(LessEq) => Op::Lte,
                some_ok_token!(Percent) => Op::Mod,
                some_ok_token!(Star) => Op::Mul,
                some_ok_token!(BangEq) => Op::Neq,
                some_ok_token!(Caret) => Op::Pow,
                some_ok_token!(Minus) => Op::Sub,
                Some(Err(e)) => return Err(e.into()),
                None => break,
                Some(Ok(Token { .. })) => break,
            };

            if let Some((l_bp, ())) = Self::postfix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                let op_token = self
                    .lexer
                    .next()
                    .expect("Already checked peek")
                    .expect("Already checked peek");

                lhs = match op {
                    Op::Call => {
                        let mut params =
                            self.parse_expr_list(TokenKind::Comma, TokenKind::RightParen)?;
                        self.expect_token(TokenKind::RightParen)?;
                        params.insert(0, lhs);
                        Expr::Cons(op, params)
                    }
                    Op::Field => Expr::Cons(op, vec![lhs]),
                    Op::Index => {
                        let rhs = self.parse_expr()?;
                        self.expect_token(TokenKind::RightBracket)?;
                        Expr::Cons(op, vec![lhs, rhs])
                    }
                    _ => {
                        // NOTE: should not be reachable due to `postfix_binding_power` returning None
                        return Err(ParserError::InvalidPostfixOperator {
                            src: self.source.to_string(),
                            span: op_token.span().into(),
                        });
                    }
                };
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
            Op::Not => ((), 9),
            Op::Pos | Op::Neg => ((), 11),
            _ => return None,
        };
        Some(res)
    }

    fn infix_binding_power(op: &Op) -> Option<(u8, u8)> {
        let res = match op {
            Op::Assign => (2, 1),
            Op::Lt | Op::Gt | Op::Lte | Op::Gte | Op::Eq | Op::Neq => (3, 4),
            Op::Add | Op::Sub => (5, 6),
            Op::Mul | Op::Div => (7, 8),
            Op::Field => (18, 17),
            _ => return None,
        };
        Some(res)
    }

    fn postfix_binding_power(op: &Op) -> Option<(u8, ())> {
        let res = match op {
            Op::Call => (15, ()),
            Op::Index => (13, ()),
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
        let mut parser = Parser::new("ret 1;");

        if let Some(Ok(expr)) = parser.next() {
            assert_eq!(format!("{}", expr), "ret 1;");
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
