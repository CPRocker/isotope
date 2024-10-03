use miette::Result;

use crate::lexer::{Lexer, Token, TokenKind};
use crate::source::Source;

mod error;
mod expression;
mod statement;

pub use error::ParserError;
pub use expression::Atom;
pub use expression::Expr;
pub use expression::Op;
pub use statement::Stmt;

pub struct Parser<'iso> {
    lexer: std::iter::Peekable<Lexer<'iso>>,
}

impl<'iso> Parser<'iso> {
    pub fn new(src: &'iso Source) -> Result<Self, ParserError> {
        Ok(Self {
            lexer: Lexer::new(src)?.peekable(),
        })
    }
}

impl<'iso> Iterator for Parser<'iso> {
    type Item = Result<Stmt, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.peek()?;
        Some(self.parse_statement())
    }
}

impl<'iso> Parser<'iso> {
    fn expect_token(&mut self, expected: TokenKind) -> Result<Token, ParserError> {
        match self.lexer.next() {
            Some(Ok(token)) if token.kind == expected => Ok(token),
            Some(Ok(token)) => Err(ParserError::ExpectedToken {
                expected: expected.to_string(),
                found: token.kind.to_string(),
                span: token.span().into(),
            }),
            Some(Err(e)) => Err(e.into()),
            None => Err(ParserError::UnexpectedEOF),
        }
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, ParserError> {
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
                        span: left_curly.span().into(),
                    });
                }
                Some(_) => statements.push(self.parse_statement()?),
            }
        }
        self.expect_token(TokenKind::RightCurly)
            .map_err(|_| ParserError::UnclosedBlock {
                span: (left_curly.span()).into(),
            })?;

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
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
                // TODO: support scoped blocks
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

    fn parse_param_list(&mut self) -> Result<Vec<statement::Identifier>, ParserError> {
        let left_paren = self.expect_token(TokenKind::LeftParen)?;
        let mut last = left_paren.clone();

        let mut params = vec![];

        loop {
            let token = match self.lexer.peek() {
                Some(Ok(token)) => token.clone(),
                Some(Err(e)) => return Err(e.into()),
                None => {
                    return Err(ParserError::UnclosedParamList {
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
                    params.push(statement::Identifier::new(orig));

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
                                span: (left_paren.offset..last.span().end).into(),
                            })
                        }
                    }
                }
                _ => {
                    return Err(ParserError::UnclosedParamList {
                        span: (left_paren.offset..last.span().end).into(),
                    });
                }
            }
        }
        self.expect_token(TokenKind::RightParen)
            .map_err(|_| ParserError::UnclosedParamList {
                span: (left_paren.offset..last.span().end).into(),
            })?;

        Ok(params)
    }

    fn parse_identifier(&mut self) -> Result<statement::Identifier, ParserError> {
        match self.lexer.next() {
            Some(Ok(Token {
                kind: TokenKind::Identifier,
                orig,
                ..
            })) => Ok(statement::Identifier::new(orig)),
            Some(Ok(Token { orig, offset, .. })) => Err(ParserError::ExpectedIdentifier {
                span: (offset..offset + orig.len()).into(),
            }),
            Some(Err(e)) => Err(e.into()),
            None => Err(ParserError::UnexpectedEOF),
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
    ) -> Result<Vec<Expr>, ParserError> {
        let mut params: Vec<Expr> = vec![];

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

    fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        if self.lexer.peek().is_none() {
            return Err(ParserError::UnexpectedEOF);
        }

        self.parse_expr_within(0) // TODO: wrap with ExpectedExpression
    }

    fn parse_expr_within(&mut self, min_bp: u8) -> Result<Expr, ParserError> {
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
            Some(Ok(token)) if token.kind == TokenKind::Number => {
                let num = token
                    .orig
                    .parse::<f64>()
                    .map_err(|e| ParserError::ParseFloatError {
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
                let s = chars.as_str().to_owned();
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
            Some(Ok(token)) => {
                return Err(ParserError::ExpectedExpression {
                    span: token.span().into(),
                });
            }
            Some(Err(e)) => return Err(e.into()),
            None => {
                return Err(ParserError::UnexpectedEOF);
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
                            op: op.to_string(),
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
    use std::str::FromStr;

    use super::*;

    #[test]
    fn eof() {
        let code = "";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut parser = Parser::new(&src).expect("failed to create parser");

        assert!(parser.next().is_none());
    }

    #[test]
    fn nop() {
        let code = ";;;";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut parser = Parser::new(&src).expect("failed to create parser");

        match parser.next() {
            Some(Ok(Stmt::Nop)) => {}
            _ => panic!("Expected `Statement::Nop`"),
        };
    }

    #[test]
    fn let_statement() {
        let code = "let x = 1;";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut parser = Parser::new(&src).expect("failed to create parser");

        match parser.next() {
            Some(Ok(Stmt::LetDeclaration { name, value })) => {
                assert_eq!(name.name(), "x");
                assert_eq!(format!("{}", value), "1");
            }
            _ => panic!("Expected `Statement::Let`"),
        };
    }

    #[test]
    fn assignment_statement() {
        let code = "x = 1;";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut parser = Parser::new(&src).expect("failed to create parser");

        match parser.next() {
            Some(Ok(Stmt::Assignment { name, value })) => {
                assert_eq!(name.name(), "x");
                assert_eq!(format!("{}", value), "1");
            }
            _ => panic!("Expected `Statement::Assignment`"),
        };
    }

    #[test]
    fn return_statement() {
        let code = "return 1;";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut parser = Parser::new(&src).expect("failed to create parser");

        match parser.next() {
            Some(Ok(Stmt::Return(expr))) => {
                assert_eq!(format!("{}", expr), "1");
            }
            _ => panic!("Expected `Statement::Return`"),
        };
    }

    #[test]
    fn break_statement() {
        let code = "break;";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut parser = Parser::new(&src).expect("failed to create parser");

        match parser.next() {
            Some(Ok(Stmt::Break)) => {}
            _ => panic!("Expected `Statement::Break`"),
        };
    }

    #[test]
    fn loop_statement() {
        let code = "loop { let x = 1; break; }";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut parser = Parser::new(&src).expect("failed to create parser");

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
        let code = "if(true) { let x = 1; }";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut parser = Parser::new(&src).expect("failed to create parser");

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
        let code = "if(true) { let x = 1; } else { let x = 2; }";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut parser = Parser::new(&src).expect("failed to create parser");

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
        let code = "if(true) { let x = 1; } else if(false) { let x = 2; }";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut parser = Parser::new(&src).expect("failed to create parser");

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
            let code = "foo(1, 2, 3)";
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut parser = Parser::new(&src).expect("failed to create parser");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "(foo(1, 2, 3))");
            } else {
                panic!("Invalid expression");
            };
        }

        #[test]
        fn indexing() {
            let code = "foo[0]";
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut parser = Parser::new(&src).expect("failed to create parser");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "([] foo 0)");
            } else {
                panic!("Invalid expression");
            };
        }

        #[test]
        fn prefix_op_precedence() {
            let code = "-1 + 2";
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut parser = Parser::new(&src).expect("failed to create parser");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "(+ (- 1) 2)");
            } else {
                panic!("Invalid expression");
            };
        }

        #[test]
        fn additive_infix_op_precedence() {
            let code = "1 + 2 - 3";
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut parser = Parser::new(&src).expect("failed to create parser");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "(- (+ 1 2) 3)");
            } else {
                panic!("Invalid expression");
            };
        }

        #[test]
        fn infix_op_precedence() {
            let code = "0 + 1 * 2 - 3 / 4";
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut parser = Parser::new(&src).expect("failed to create parser");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "(- (+ 0 (* 1 2)) (/ 3 4))");
            } else {
                panic!("Invalid expression");
            };
        }

        #[test]
        fn infix_postfix_op_precedence() {
            let code = "1 + foo[0] - 3";
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut parser = Parser::new(&src).expect("failed to create parser");

            if let Ok(expr) = parser.parse_expr() {
                assert_eq!(format!("{}", expr), "(- (+ 1 ([] foo 0)) 3)");
            } else {
                panic!("Invalid expression");
            };
        }

        #[test]
        fn postfix_op_precedence() {
            let code = "foo(1, 2, 3)[0]";
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut parser = Parser::new(&src).expect("failed to create parser");

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
            let code = "fn foo() { let x = 1;";
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut parser = Parser::new(&src).expect("failed to create parser");

            match parser.next() {
                Some(Err(ParserError::UnclosedBlock { span, .. })) => {
                    assert_eq!(span, (9..10).into())
                }
                _ => panic!("Expected `ParserError::UnclosedBlock`"),
            }
        }

        #[test]
        fn unclosed_param_list() {
            let code = "fn foo(x, y { let x = 1;";
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut parser = Parser::new(&src).expect("failed to create parser");

            match parser.next() {
                Some(Err(ParserError::UnclosedParamList { span, .. })) => {
                    assert_eq!(span, (6..11).into())
                }
                _ => panic!("Expected `ParserError::UnclosedParamList`"),
            }
        }

        #[test]
        fn let_without_identifier() {
            let code = "let = 1;";
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut parser = Parser::new(&src).expect("failed to create parser");

            match parser.next() {
                Some(Err(ParserError::ExpectedIdentifier { span, .. })) => {
                    assert_eq!(span, (4..5).into())
                }
                _ => panic!("Expected `ParserError::ExpectedIdentifier`"),
            }
        }

        #[test]
        fn let_without_expression() {
            let code = "let x = ;";
            let src = Source::from_str(code).expect("code is valid UTF-8");
            let mut parser = Parser::new(&src).expect("failed to create parser");

            match parser.next() {
                Some(Err(ParserError::ExpectedExpression { span, .. })) => {
                    assert_eq!(span, (8..9).into())
                }
                _ => panic!("Expected `ParserError::ExpectedExpression`"),
            }
        }
    }
}
