use crate::lexer::Lexer;

pub struct Parser<'de> {
    lexer: Lexer<'de>,
}

impl<'de> Parser<'de> {
    pub fn new(file_contents: &'de str) -> Self {
        Self {
            lexer: Lexer::new(file_contents),
        }
    }
}

// #[derive(Debug)]
// pub struct Program {
//     statements: Vec<Statement>,
// }

// impl Program {
//     pub fn new() -> Self {
//         Self { statements: vec![] }
//     }

//     pub fn add_statement(&mut self, statement: Statement) {
//         self.statements.push(statement);
//     }
// }

// impl IntoIterator for Program {
//     type Item = Statement;

//     type IntoIter = std::vec::IntoIter<Statement>;

//     fn into_iter(self) -> Self::IntoIter {
//         self.statements.into_iter()
//     }
// }

// #[derive(Debug)]
// pub enum Statement {
//     Return {
//         expression: Expression,
//     },
//     VariableDeclaration {
//         identifier: String,
//         expression: Expression,
//     },
// }

// #[derive(Debug)]
// pub enum Expression {
//     Binary(BinaryExpression),
//     Identifier(String),
//     Literal(Literal),
// }

// #[derive(Debug)]
// pub enum BinaryExpression {
//     Additive(Box<Expression>, BinaryOperator, Box<Expression>),
//     Multiplicative(Box<Expression>, BinaryOperator, Box<Expression>),
// }

// #[derive(Debug)]
// pub enum BinaryOperator {
//     Add,
//     Sub,
//     Mul,
//     Div,
// }

// impl BinaryOperator {
//     pub fn get_precedence(&self) -> u8 {
//         match self {
//             BinaryOperator::Add | BinaryOperator::Sub => 1,
//             BinaryOperator::Mul | BinaryOperator::Div => 2,
//         }
//     }
// }

// #[derive(Debug)]
// pub enum Literal {
//     IntLiteral { value: i64 },
// }

// const END_STATEMENT: Statement = Statement::Return {
//     expression: Expression::Literal(Literal::IntLiteral { value: 0 }),
// };

// pub fn parse(mut tokens: VecDeque<Token>) -> Result<Program, error::ParsingError> {
//     let mut program = Program::new();
//     let mut declared_vars: HashSet<String> = HashSet::new();

//     while let Some(token) = tokens.front() {
//         let (statement, remaining_tokens) = match token {
//             Token::Eof => (END_STATEMENT, VecDeque::new()),
//             _ => parse_statement(tokens, &mut declared_vars)?,
//         };
//         program.add_statement(statement);
//         tokens = remaining_tokens;
//     }

//     Ok(program)
// }

// fn parse_statement(
//     mut tokens: VecDeque<Token>,
//     declared_vars: &mut HashSet<String>,
// ) -> Result<(Statement, VecDeque<Token>), error::ParsingError> {
//     match tokens.front() {
//         Some(Token::Let) => {
//             tokens.pop_front();

//             let (identifier, remaining_tokens) = parse_identifier(tokens)?;
//             tokens = remaining_tokens;
//             declared_vars.insert(identifier.clone());

//             tokens = try_consume(tokens, Token::Equals)?;

//             let (expression, remaining_tokens) = parse_expression(tokens, declared_vars, 0)?;
//             tokens = remaining_tokens;

//             tokens = try_consume(tokens, Token::Semi)?;

//             Ok((
//                 Statement::VariableDeclaration {
//                     identifier,
//                     expression,
//                 },
//                 tokens,
//             ))
//         }
//         Some(Token::Return) => {
//             tokens.pop_front();

//             let (expression, remaining_tokens) = parse_expression(tokens, declared_vars, 0)?;
//             tokens = remaining_tokens;

//             tokens = try_consume(tokens, Token::Semi)?;

//             Ok((Statement::Return { expression }, tokens))
//         }
//         Some(_) => Err(error::ParsingError::UnexpectedToken(
//             tokens.pop_front().unwrap(),
//         )),
//         None => Err(error::ParsingError::Statement),
//     }
// }

// fn parse_identifier(
//     mut tokens: VecDeque<Token>,
// ) -> Result<(String, VecDeque<Token>), error::ParsingError> {
//     if let Some(Token::Identifier(identifier)) = tokens.pop_front() {
//         Ok((identifier, tokens))
//     } else {
//         Err(error::ParsingError::ExpectedIdentifier)
//     }
// }

// fn parse_expression(
//     mut tokens: VecDeque<Token>,
//     declared_vars: &HashSet<String>,
//     precedence: u8,
// ) -> Result<(Expression, VecDeque<Token>), error::ParsingError> {
//     let (mut left_expression, remaining_tokens) = parse_primary_expression(tokens, declared_vars)?;
//     tokens = remaining_tokens;

//     while let Some(token) = tokens.front() {
//         match token {
//             Token::Plus | Token::Minus | Token::Star | Token::Slash => {
//                 let operator_token = tokens.pop_front().unwrap();
//                 let operator = match operator_token {
//                     Token::Plus => BinaryOperator::Add,
//                     Token::Minus => BinaryOperator::Sub,
//                     Token::Star => BinaryOperator::Mul,
//                     Token::Slash => BinaryOperator::Div,
//                     _ => unreachable!(),
//                 };

//                 let operator_precedence = operator.get_precedence();
//                 if operator_precedence < precedence {
//                     break;
//                 }

//                 let (right_expression, remaining_tokens) =
//                     parse_primary_expression(tokens, declared_vars)?;
//                 tokens = remaining_tokens;

//                 left_expression = match operator {
//                     BinaryOperator::Add | BinaryOperator::Sub => {
//                         Expression::Binary(BinaryExpression::Additive(
//                             Box::new(left_expression),
//                             operator,
//                             Box::new(right_expression),
//                         ))
//                     }
//                     BinaryOperator::Mul | BinaryOperator::Div => {
//                         Expression::Binary(BinaryExpression::Multiplicative(
//                             Box::new(left_expression),
//                             operator,
//                             Box::new(right_expression),
//                         ))
//                     }
//                 };
//             }
//             _ => break,
//         }
//     }

//     Ok((left_expression, tokens))
// }

// fn parse_primary_expression(
//     mut tokens: VecDeque<Token>,
//     declared_vars: &HashSet<String>,
// ) -> Result<(Expression, VecDeque<Token>), error::ParsingError> {
//     match tokens.pop_front() {
//         Some(Token::LeftParen) => {
//             let (inner_expression, remaining_tokens) = parse_expression(tokens, declared_vars, 0)?;
//             tokens = remaining_tokens;

//             tokens = try_consume(tokens, Token::RightParen)?;

//             Ok((inner_expression, tokens))
//         }
//         Some(Token::Identifier(identifier)) => match declared_vars.contains(&identifier) {
//             true => Ok((Expression::Identifier(identifier), tokens)),
//             false => Err(error::ParsingError::UndeclaredIndentifier(identifier)),
//         },
//         Some(Token::Literal(value)) => {
//             let literal = parse_literal(&value)?;
//             Ok((Expression::Literal(literal), tokens))
//         }
//         _ => Err(error::ParsingError::ExpectedExpression),
//     }
// }

// fn parse_literal(literal: &str) -> Result<Literal, error::ParsingError> {
//     let int_re = regex::Regex::new(r"\d+").unwrap();

//     if int_re.is_match(literal) {
//         return Ok(Literal::IntLiteral {
//             value: literal.parse::<i64>().unwrap(),
//         });
//     }

//     Err(error::ParsingError::Literal(String::from(literal)))
// }

// fn try_consume(
//     mut tokens: VecDeque<Token>,
//     expected: Token,
// ) -> Result<VecDeque<Token>, error::ParsingError> {
//     match tokens.pop_front() {
//         Some(token) if token == expected => Ok(tokens),
//         _ => Err(error::ParsingError::ExpectedToken(format!(
//             "{:?}",
//             expected
//         ))),
//     }
// }
