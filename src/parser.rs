extern crate regex;

use std::collections::VecDeque;

use crate::tokenizer::tokens::Token;

pub mod expressions;
pub mod statements;

pub fn parse(mut tokens: VecDeque<Token>) -> Result<statements::Program, String> {
    let mut program = statements::Program::new();

    while !tokens.is_empty() {
        let token_result = match parse_statement(tokens) {
            Ok((statement, tokens)) => (statement, tokens),
            Err(e) => return Err(e),
        };
        tokens = token_result.1;

        program.add_statement(token_result.0);
    }

    Ok(program)
}

fn parse_statement(
    mut tokens: VecDeque<Token>,
) -> Result<(statements::Statement, VecDeque<Token>), String> {
    match tokens.front() {
        Some(Token::Return) => {
            tokens.pop_front();

            match parse_expression(tokens) {
                Ok((expression, mut tokens)) => match tokens.front() {
                    Some(Token::Semi) => {
                        tokens.pop_front();

                        Ok((statements::Statement::Return { expression }, tokens))
                    }
                    _ => Err(String::from("Expected `;`")),
                },
                Err(e) => Err(e),
            }
        }
        Some(token) => Err(format!("Unexpected token: {:?}", token)),
        None => Err(String::from("Unable to parse statement")),
    }
}

fn parse_expression(
    mut tokens: VecDeque<Token>,
) -> Result<(expressions::Expression, VecDeque<Token>), String> {
    match tokens.pop_front() {
        Some(Token::Literal(value)) => match parse_literal(&value) {
            Ok(literal) => Ok((expressions::Expression::Literal(literal), tokens)),
            Err(e) => Err(e),
        },
        _ => Err(String::from("Expected expression")),
    }
}

fn parse_literal(literal: &str) -> Result<expressions::Literal, String> {
    let int_re = regex::Regex::new(r"\d+").unwrap();

    if int_re.is_match(literal) {
        return Ok(expressions::Literal::IntLiteral {
            value: literal.parse::<i64>().unwrap(),
        });
    }

    Err(format!("Could not parse literal: {}", literal))
}
