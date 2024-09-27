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
    ParseFloatError {
        #[source_code]
        src: String,
        #[label = "here"]
        span: SourceSpan,
        #[source]
        error: std::num::ParseFloatError,
    },
    UnexpectedEOF {
        #[source_code]
        src: String,
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
    InvalidElseStatement {
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
            ParserError::ParseFloatError { .. } => {
                write!(f, "Failed to parse floating point number")
            }
            ParserError::UnexpectedEOF { .. } => write!(f, "Unexpected EOF"),
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
            ParserError::InvalidElseStatement { .. } => {
                write!(
                    f,
                    "Invalid else statement: expected `else if` or `else` block"
                )
            }
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
pub enum Stmt<'iso> {
    Assignment {
        name: Identifier<'iso>,
        value: Expr<'iso>,
    },
    Break,
    Expr(Expr<'iso>),
    FunctionDeclaration {
        name: Identifier<'iso>,
        params: Vec<Identifier<'iso>>,
        body: Vec<Stmt<'iso>>,
    },
    If {
        condition: Expr<'iso>,
        body: Vec<Stmt<'iso>>,
        else_body: Option<Vec<Stmt<'iso>>>,
    },
    LetDeclaration {
        name: Identifier<'iso>,
        value: Expr<'iso>,
    },
    Loop {
        body: Vec<Stmt<'iso>>,
    },
    Nop,
    Return(Expr<'iso>),
}

impl Display for Stmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Assignment { name, value } => write!(f, "{} = {};", name, value),
            Stmt::Break => write!(f, "break;"),
            Stmt::Expr(expr) => write!(f, "{}", expr),
            Stmt::FunctionDeclaration { name, params, body } => {
                write!(f, "fn {}(", name)?;
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
            Stmt::If {
                condition,
                body,
                else_body,
            } => {
                write!(f, "if({}) {{ ", condition)?;
                for statement in body {
                    write!(f, "{} ", statement)?;
                }
                write!(f, "}}")?;
                if let Some(else_body) = else_body {
                    write!(f, " else {{ ")?;
                    for statement in else_body {
                        write!(f, "{} ", statement)?;
                    }
                    write!(f, "}}")?;
                }
                Ok(())
            }
            Stmt::LetDeclaration { name, value } => write!(f, "let {} = {};", name, value),
            Stmt::Loop { body } => {
                write!(f, "loop {{ ")?;
                for statement in body {
                    write!(f, "{} ", statement)?;
                }
                write!(f, "}}")
            }
            Stmt::Nop => write!(f, "nop;"),
            Stmt::Return(expr) => write!(f, "return {};", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier<'iso> {
    name: &'iso str,
}

impl Display for Identifier<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'iso> {
    Atom(Atom<'iso>),
    Cons(Op, Vec<Expr<'iso>>),
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Atom(Atom::Boolean(b)) => write!(f, "{}", b),
            Expr::Atom(Atom::Identifier(s)) => write!(f, "{}", s),
            Expr::Atom(Atom::Number(n)) => write!(f, "{}", n),
            Expr::Atom(Atom::String(s)) => write!(f, "\"{}\"", s),
            Expr::Cons(op, args) if op == &Op::Call => {
                for (i, arg) in args.iter().enumerate() {
                    if i == 0 {
                        write!(f, "({}(", arg)?;
                    } else if i == 1 {
                        write!(f, "{}", arg)?;
                    } else {
                        write!(f, ", {}", arg)?;
                    }
                }
                write!(f, "))")
            }
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
pub enum Atom<'iso> {
    Boolean(bool),
    Identifier(&'iso str),
    Number(f64),
    String(&'iso str),
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

pub struct Parser<'iso> {
    src: &'iso str,
    lexer: Peekable<Lexer<'iso>>,
}

impl<'iso> Parser<'iso> {
    pub fn new(file_contents: &'iso str) -> Self {
        Self {
            src: file_contents,
            lexer: Lexer::new(file_contents).peekable(),
        }
    }
}

impl<'iso> Iterator for Parser<'iso> {
    type Item = Result<Stmt<'iso>, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.peek()?;
        Some(self.parse_statement())
    }
}

impl<'iso> Parser<'iso> {
    fn expect_token(&mut self, expected: TokenKind) -> Result<Token<'iso>, ParserError> {
        match self.lexer.next() {
            Some(Ok(token @ Token { kind, .. })) => {
                if kind == expected {
                    return Ok(token);
                }

                Err(ParserError::ExpectedToken {
                    src: self.src.to_string(),
                    span: token.span().into(),
                    expected: format!("{}", expected),
                })
            }
            Some(Err(e)) => Err(e.into()),
            None => Err(ParserError::UnexpectedEOF {
                src: self.src.to_string(),
            }),
        }
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt<'iso>>, ParserError> {
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
                        src: self.src.to_string(),
                        span: left_curly.span().into(),
                    });
                }
                Some(_) => statements.push(self.parse_statement()?),
            }
        }
        self.expect_token(TokenKind::RightCurly)
            .map_err(|_| ParserError::UnclosedBlock {
                src: self.src.to_string(),
                span: (left_curly.span()).into(),
            })?;

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Stmt<'iso>, ParserError> {
        match self.lexer.peek().expect("peek already checked") {
            Ok(Token {
                kind: TokenKind::Semicolon,
                ..
            }) => {
                self.lexer.next();
                Ok(Stmt::Nop)
            }
            Ok(Token {
                kind: TokenKind::LeftCurly,
                ..
            }) => {
                todo!()
            }
            Ok(Token {
                kind: TokenKind::Break,
                ..
            }) => {
                self.lexer.next();
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Stmt::Break)
            }
            Ok(Token {
                kind: TokenKind::Function,
                ..
            }) => {
                self.lexer.next();
                let name = self.parse_identifier()?;
                let params = self.parse_param_list()?;
                let body = self.parse_block()?;
                Ok(Stmt::FunctionDeclaration { name, params, body })
            }
            Ok(Token {
                kind: TokenKind::Identifier,
                ..
            }) => {
                // TODO: support function calls on identifier or as expression
                let name = self.parse_identifier()?;
                self.expect_token(TokenKind::Equal)?;
                let value = self.parse_expr()?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Stmt::Assignment { name, value })
            }
            Ok(Token {
                kind: TokenKind::If,
                ..
            }) => {
                self.lexer.next();
                self.expect_token(TokenKind::LeftParen)?;
                let condition = self.parse_expr()?;
                self.expect_token(TokenKind::RightParen)?;
                let body = self.parse_block()?;

                let else_token = if let Some(Ok(Token {
                    kind: TokenKind::Else,
                    ..
                })) = self.lexer.peek()
                {
                    self.lexer
                        .next()
                        .expect("Checked on peek")
                        .expect("Checked on peek")
                } else {
                    return Ok(Stmt::If {
                        condition,
                        body,
                        else_body: None,
                    });
                };

                match self.lexer.peek() {
                    Some(Ok(Token {
                        kind: TokenKind::If,
                        ..
                    })) => Ok(Stmt::If {
                        condition,
                        body,
                        else_body: Some(vec![self.parse_statement()?]),
                    }),
                    Some(Ok(Token {
                        kind: TokenKind::LeftCurly,
                        ..
                    })) => Ok(Stmt::If {
                        condition,
                        body,
                        else_body: Some(self.parse_block()?),
                    }),
                    Some(Err(e)) => Err(e.into()),
                    _ => Err(ParserError::InvalidElseStatement {
                        src: self.src.to_string(),
                        span: else_token.span().into(),
                    }),
                }
            }
            Ok(Token {
                kind: TokenKind::Let,
                ..
            }) => {
                self.lexer.next();
                let name = self.parse_identifier()?;
                self.expect_token(TokenKind::Equal)?;
                let value = self.parse_expr()?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Stmt::LetDeclaration { name, value })
            }
            Ok(Token {
                kind: TokenKind::Loop,
                ..
            }) => {
                self.lexer.next();
                let body = self.parse_block()?;
                Ok(Stmt::Loop { body })
            }
            Ok(Token {
                kind: TokenKind::Return,
                ..
            }) => {
                self.lexer.next();
                let expr = self.parse_expr()?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Stmt::Return(expr))
            }
            Ok(Token {
                kind: TokenKind::Else,
                ..
            }) => {
                todo!("Invalid start of statement")
            }
            Ok(_) => self.parse_expr().map(Stmt::Expr),
            Err(e) => Err(e.into()),
        }
    }

    fn parse_param_list(&mut self) -> Result<Vec<Identifier<'iso>>, ParserError> {
        let left_paren = self.expect_token(TokenKind::LeftParen)?;
        let mut last = left_paren.clone();

        let mut params = vec![];

        loop {
            let token = match self.lexer.peek() {
                Some(Ok(token)) => token.clone(),
                Some(Err(e)) => return Err(e.into()),
                None => {
                    return Err(ParserError::UnclosedParamList {
                        src: self.src.to_string(),
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
                    kind: TokenKind::Identifier,
                    orig,
                    ..
                } => {
                    last = self
                        .lexer
                        .next()
                        .expect("Checked on peek")
                        .expect("Checked on peek");
                    params.push(Identifier { name: orig });

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
                                src: self.src.to_string(),
                                span: (left_paren.offset..last.span().end).into(),
                            })
                        }
                    }
                }
                _ => {
                    return Err(ParserError::UnclosedParamList {
                        src: self.src.to_string(),
                        span: (left_paren.offset..last.span().end).into(),
                    });
                }
            }
        }
        self.expect_token(TokenKind::RightParen)
            .map_err(|_| ParserError::UnclosedParamList {
                src: self.src.to_string(),
                span: (left_paren.offset..last.span().end).into(),
            })?;

        Ok(params)
    }

    fn parse_identifier(&mut self) -> Result<Identifier<'iso>, ParserError> {
        match self.lexer.next() {
            Some(Ok(Token {
                kind: TokenKind::Identifier,
                orig,
                ..
            })) => Ok(Identifier { name: orig }),
            Some(Ok(Token { orig, offset, .. })) => Err(ParserError::ExpectedIdentifier {
                src: self.src.to_string(),
                span: (offset..offset + orig.len()).into(),
            }),
            Some(Err(e)) => Err(e.into()),
            None => Err(ParserError::UnexpectedEOF {
                src: self.src.to_string(),
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
    ) -> Result<Vec<Expr<'iso>>, ParserError> {
        let mut params: Vec<Expr<'iso>> = vec![];

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

    fn parse_expr(&mut self) -> Result<Expr<'iso>, ParserError> {
        if self.lexer.peek().is_none() {
            return Err(ParserError::UnexpectedEOF {
                src: self.src.to_string(),
            });
        }

        self.parse_expr_within(0) // TODO: wrap with ExpectedExpression
    }

    fn parse_expr_within(&mut self, min_bp: u8) -> Result<Expr<'iso>, ParserError> {
        let mut lhs = match self.lexer.next() {
            // literals
            Some(Ok(Token {
                kind: TokenKind::False,
                ..
            })) => Expr::Atom(Atom::Boolean(false)),
            Some(Ok(Token {
                kind: TokenKind::Identifier,
                orig,
                ..
            })) => Expr::Atom(Atom::Identifier(orig)),
            Some(Ok(
                token @ Token {
                    kind: TokenKind::Number,
                    orig,
                    ..
                },
            )) => {
                let num = orig
                    .parse::<f64>()
                    .map_err(|e| ParserError::ParseFloatError {
                        src: self.src.to_string(),
                        span: token.span().into(),
                        error: e,
                    })?;
                // TODO: add other number types

                Expr::Atom(Atom::Number(num))
            }
            Some(Ok(Token {
                kind: TokenKind::String,
                orig,
                ..
            })) => {
                let mut chars = orig.chars();
                chars.next();
                chars.next_back();
                let s = chars.as_str();
                // TODO: escape sequences

                Expr::Atom(Atom::String(s))
            }
            Some(Ok(Token {
                kind: TokenKind::True,
                ..
            })) => Expr::Atom(Atom::Boolean(true)),
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
                    src: self.src.to_string(),
                });
            }
            Some(Ok(token)) => {
                return Err(ParserError::ExpectedExpression {
                    src: self.src.to_string(),
                    span: token.span().into(),
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
                some_ok_token!(Equal) => Op::Assign,
                some_ok_token!(LeftParen) => Op::Call,
                some_ok_token!(Slash) => Op::Div,
                some_ok_token!(EqualEqual) => Op::Eq,
                some_ok_token!(Dot) => Op::Field,
                some_ok_token!(Greater) => Op::Gt,
                some_ok_token!(GreaterEqual) => Op::Gte,
                some_ok_token!(LeftBracket) => Op::Index,
                some_ok_token!(Less) => Op::Lt,
                some_ok_token!(LessEqual) => Op::Lte,
                some_ok_token!(Percent) => Op::Mod,
                some_ok_token!(Star) => Op::Mul,
                some_ok_token!(BangEqual) => Op::Neq,
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
                            src: self.src.to_string(),
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
mod tests {
    use super::*;

    #[test]
    fn eof() {
        let mut parser = Parser::new("");

        assert!(parser.next().is_none());
    }

    #[test]
    fn nop() {
        let mut parser = Parser::new(";;;");

        match parser.next() {
            Some(Ok(Stmt::Nop)) => {}
            _ => panic!("Expected `Statement::Nop`"),
        };
    }

    #[test]
    fn let_statement() {
        let mut parser = Parser::new("let x = 1;");

        match parser.next() {
            Some(Ok(Stmt::LetDeclaration { name, value })) => {
                assert_eq!(name.name, "x");
                assert_eq!(format!("{}", value), "1");
            }
            _ => panic!("Expected `Statement::Let`"),
        };
    }

    #[test]
    fn assignment_statement() {
        let mut parser = Parser::new("x = 1;");

        match parser.next() {
            Some(Ok(Stmt::Assignment { name, value })) => {
                assert_eq!(name.name, "x");
                assert_eq!(format!("{}", value), "1");
            }
            _ => panic!("Expected `Statement::Assignment`"),
        };
    }

    #[test]
    fn return_statement() {
        let mut parser = Parser::new("return 1;");

        match parser.next() {
            Some(Ok(Stmt::Return(expr))) => {
                assert_eq!(format!("{}", expr), "1");
            }
            _ => panic!("Expected `Statement::Return`"),
        };
    }

    #[test]
    fn break_statement() {
        let mut parser = Parser::new("break;");

        match parser.next() {
            Some(Ok(Stmt::Break)) => {}
            _ => panic!("Expected `Statement::Break`"),
        };
    }

    #[test]
    fn loop_statement() {
        let mut parser = Parser::new("loop { let x = 1; break; }");

        match parser.next() {
            Some(Ok(Stmt::Loop { body })) => {
                assert_eq!(body.len(), 2);
                assert_eq!(format!("{}", body[0]), "let x = 1;");
                assert_eq!(format!("{}", body[1]), "break;");
            }
            _ => panic!("Expected `Statement::Loop`"),
        };
    }

    #[test]
    fn if_statement() {
        let mut parser = Parser::new("if(true) { let x = 1; }");

        match parser.next() {
            Some(Ok(Stmt::If {
                condition,
                body,
                else_body,
            })) => {
                assert_eq!(format!("{}", condition), "true");
                assert_eq!(body.len(), 1);
                assert_eq!(format!("{}", body[0]), "let x = 1;");
                assert!(else_body.is_none());
            }
            _ => panic!("Expected `Statement::If`"),
        };
    }

    #[test]
    fn if_else_statement() {
        let mut parser = Parser::new("if(true) { let x = 1; } else { let x = 2; }");

        match parser.next() {
            Some(Ok(Stmt::If {
                condition,
                body,
                else_body,
            })) => {
                assert_eq!(format!("{}", condition), "true");
                assert_eq!(body.len(), 1);
                assert_eq!(format!("{}", body[0]), "let x = 1;");
                if let Some(else_body) = else_body {
                    assert_eq!(else_body.len(), 1);
                    assert_eq!(format!("{}", else_body[0]), "let x = 2;");
                } else {
                    panic!("Expected `else_body`");
                }
            }
            _ => panic!("Expected `Statement::If`"),
        };
    }

    #[test]
    fn if_else_if_statement() {
        let mut parser = Parser::new("if(true) { let x = 1; } else if(false) { let x = 2; }");

        match parser.next() {
            Some(Ok(Stmt::If {
                condition,
                body,
                else_body,
            })) => {
                assert_eq!(format!("{}", condition), "true");
                assert_eq!(body.len(), 1);
                assert_eq!(format!("{}", body[0]), "let x = 1;");
                if let Some(else_body) = else_body {
                    assert_eq!(else_body.len(), 1);
                    assert_eq!(format!("{}", else_body[0]), "if(false) { let x = 2; }");
                } else {
                    panic!("Expected `else_body`");
                }
            }
            _ => panic!("Expected `Statement::If`"),
        };
    }

    mod expr {
        use super::*;

        #[test]
        fn function_call() {
            let mut parser = Parser::new("foo(1, 2, 3)");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "(foo(1, 2, 3))");
            } else {
                panic!("Invalid expression");
            };
        }

        #[test]
        fn indexing() {
            let mut parser = Parser::new("foo[0]");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "([] foo 0)");
            } else {
                panic!("Invalid expression");
            };
        }

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
        fn additive_infix_op_precedence() {
            let mut parser = Parser::new("1 + 2 - 3");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "(- (+ 1 2) 3)");
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

        #[test]
        fn infix_postfix_op_precedence() {
            let mut parser = Parser::new("1 + foo[0] - 3");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "(- (+ 1 ([] foo 0)) 3)");
            } else {
                panic!("Invalid expression");
            };
        }

        #[test]
        fn postfix_op_precedence() {
            let mut parser = Parser::new("foo(1, 2, 3)[0]");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "([] (foo(1, 2, 3)) 0)");
            } else {
                panic!("Invalid expression");
            };
        }
    }

    mod errors {
        use super::*;

        #[test]
        fn unclosed_block() {
            let src = "fn foo() { let x = 1;";
            let mut parser = Parser::new(src);

            match parser.next() {
                Some(Err(ParserError::UnclosedBlock { span, .. })) => {
                    assert_eq!(span, (9..10).into())
                }
                _ => panic!("Expected `ParserError::UnclosedBlock`"),
            }
        }

        #[test]
        fn unclosed_param_list() {
            let src = "fn foo(x, y { let x = 1;";
            let mut parser = Parser::new(src);

            match parser.next() {
                Some(Err(ParserError::UnclosedParamList { span, .. })) => {
                    assert_eq!(span, (6..11).into())
                }
                _ => panic!("Expected `ParserError::UnclosedParamList`"),
            }
        }

        #[test]
        fn let_without_identifier() {
            let src = "let = 1;";
            let mut parser = Parser::new(src);

            match parser.next() {
                Some(Err(ParserError::ExpectedIdentifier { span, .. })) => {
                    assert_eq!(span, (4..5).into())
                }
                _ => panic!("Expected `ParserError::ExpectedIdentifier`"),
            }
        }

        #[test]
        fn let_without_expression() {
            let src = "let x = ;";
            let mut parser = Parser::new(src);

            match parser.next() {
                Some(Err(ParserError::ExpectedExpression { span, .. })) => {
                    assert_eq!(span, (8..9).into())
                }
                _ => panic!("Expected `ParserError::ExpectedExpression`"),
            }
        }
    }
}
