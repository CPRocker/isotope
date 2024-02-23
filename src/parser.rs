extern crate regex;

use std::collections::VecDeque;

use crate::{error, tokenizer::tokens::Token};

use self::expressions::BinaryOperator;

pub mod expressions;
pub mod statements;

const END_STATEMENT: statements::Statement = statements::Statement::Return {
    expression: expressions::Expression::Literal(expressions::Literal::IntLiteral { value: 0 }),
};

pub fn parse(mut tokens: VecDeque<Token>) -> Result<statements::Program, error::ParsingError> {
    let mut program = statements::Program::new();

    while let Some(token) = tokens.front() {
        let (statement, remaining_tokens) = match token {
            Token::Eof => (END_STATEMENT, VecDeque::new()),
            _ => parse_statement(tokens)?,
        };
        program.add_statement(statement);
        tokens = remaining_tokens;
    }

    Ok(program)
}

fn parse_statement(
    mut tokens: VecDeque<Token>,
) -> Result<(statements::Statement, VecDeque<Token>), error::ParsingError> {
    match tokens.front() {
        Some(Token::Return) => {
            tokens.pop_front();

            let (expression, remaining_tokens) = parse_expression(tokens, 0)?;
            tokens = remaining_tokens;

            tokens = try_consume(tokens, Token::Semi)?;

            Ok((statements::Statement::Return { expression }, tokens))
        }
        Some(_) => Err(error::ParsingError::UnexpectedToken(
            tokens.pop_front().unwrap(),
        )),
        None => Err(error::ParsingError::Statement),
    }
}

fn parse_expression(
    mut tokens: VecDeque<Token>,
    precedence: u8,
) -> Result<(expressions::Expression, VecDeque<Token>), error::ParsingError> {
    let (mut left_expression, remaining_tokens) = parse_primary_expression(tokens)?;
    tokens = remaining_tokens;

    while let Some(token) = tokens.front() {
        match token {
            Token::Plus | Token::Minus | Token::Star | Token::Slash => {
                let operator_token = tokens.pop_front().unwrap();
                let operator = match operator_token {
                    Token::Plus => BinaryOperator::Add,
                    Token::Minus => BinaryOperator::Sub,
                    Token::Star => BinaryOperator::Mul,
                    Token::Slash => BinaryOperator::Div,
                    _ => unreachable!(),
                };

                let operator_precedence = operator.get_precedence();
                if operator_precedence < precedence {
                    break;
                }

                let (right_expression, remaining_tokens) = parse_primary_expression(tokens)?;
                tokens = remaining_tokens;

                left_expression = match operator {
                    BinaryOperator::Add | BinaryOperator::Sub => {
                        expressions::Expression::Binary(expressions::BinaryExpression::Additive(
                            Box::new(left_expression),
                            operator,
                            Box::new(right_expression),
                        ))
                    }
                    BinaryOperator::Mul | BinaryOperator::Div => expressions::Expression::Binary(
                        expressions::BinaryExpression::Multiplicative(
                            Box::new(left_expression),
                            operator,
                            Box::new(right_expression),
                        ),
                    ),
                };
            }
            _ => break,
        }
    }

    Ok((left_expression, tokens))
}

fn parse_primary_expression(
    mut tokens: VecDeque<Token>,
) -> Result<(expressions::Expression, VecDeque<Token>), error::ParsingError> {
    match tokens.pop_front() {
        Some(Token::Literal(value)) => {
            let literal = parse_literal(&value)?;
            Ok((expressions::Expression::Literal(literal), tokens))
        }
        Some(Token::LeftParen) => {
            let (inner_expression, remaining_tokens) = parse_expression(tokens, 0)?;
            tokens = remaining_tokens;

            tokens = try_consume(tokens, Token::RightParen)?;

            Ok((inner_expression, tokens))
        }
        _ => Err(error::ParsingError::ExpectedExpression),
    }
}

fn parse_literal(literal: &str) -> Result<expressions::Literal, error::ParsingError> {
    let int_re = regex::Regex::new(r"\d+").unwrap();

    if int_re.is_match(literal) {
        return Ok(expressions::Literal::IntLiteral {
            value: literal.parse::<i64>().unwrap(),
        });
    }

    Err(error::ParsingError::Literal(String::from(literal)))
}

fn try_consume(
    mut tokens: VecDeque<Token>,
    expected: Token,
) -> Result<VecDeque<Token>, error::ParsingError> {
    match tokens.pop_front() {
        Some(token) if token == expected => Ok(tokens),
        _ => Err(error::ParsingError::ExpectedToken(format!(
            "{:?}",
            expected
        ))),
    }
}
