use std::collections::VecDeque;

use crate::tokenizer::tokens::Token;

pub mod expressions;
pub mod statements;

use self::statements::Statement;

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

                        Ok((Statement::Return { expression }, tokens))
                    }
                    _ => Err(String::from("Expected `;`")),
                },
                Err(e) => Err(e),
            }
        }
        Some(_) => Err(String::from("Unexpected token")),
        None => Err(String::from("Unable to parse statement")),
    }
}

fn parse_expression(
    mut tokens: VecDeque<Token>,
) -> Result<(expressions::Expression, VecDeque<Token>), String> {
    match tokens.pop_front() {
        Some(Token::Literal(s)) => match parse_literal(s) {
            Ok(literal) => Ok((expressions::Expression::Literal(literal), tokens)),
            Err(e) => Err(e),
        },
        _ => Err(String::from("Expected expression")),
    }
}

fn parse_literal(literal: String) -> Result<expressions::Literal, String> {
    match literal.as_str() {
        "0" => Ok(expressions::Literal::IntLiteral { value: 0 }),
        "1" => Ok(expressions::Literal::IntLiteral { value: 1 }),
        "" => Err(String::from("Empty literal")),
        _ => Err(format!("Could not parse literal: {}", literal)),
    }
}
