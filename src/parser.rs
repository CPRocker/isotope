extern crate regex;

use std::collections::VecDeque;

use crate::{error, tokenizer::tokens::Token};

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

            let expression: expressions::Expression;
            (expression, tokens) = parse_expression(tokens)?;

            match tokens.front() {
                Some(Token::Semi) => {
                    tokens.pop_front();

                    Ok((statements::Statement::Return { expression }, tokens))
                }
                _ => Err(error::ParsingError::ExpectedToken(String::from(";"))),
            }
        }
        Some(_) => Err(error::ParsingError::UnexpectedToken(
            tokens.pop_front().unwrap(),
        )),
        None => Err(error::ParsingError::Statement),
    }
}

fn parse_expression(
    mut tokens: VecDeque<Token>,
) -> Result<(expressions::Expression, VecDeque<Token>), error::ParsingError> {
    match tokens.pop_front() {
        Some(Token::Literal(value)) => {
            let literal = parse_literal(&value)?;
            Ok((expressions::Expression::Literal(literal), tokens))
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
