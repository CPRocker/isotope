#[derive(Debug)]
pub enum Expression {
    Binary(BinaryExpression),
    Literal(Literal),
}

#[derive(Debug)]
pub enum BinaryExpression {
    Additive(Box<Expression>, BinaryOperator, Box<Expression>),
    Multiplicative(Box<Expression>, BinaryOperator, Box<Expression>),
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinaryOperator {
    pub fn get_precedence(&self) -> u8 {
        match self {
            BinaryOperator::Add | BinaryOperator::Sub => 1,
            BinaryOperator::Mul | BinaryOperator::Div => 2,
        }
    }
}

#[derive(Debug)]
pub enum Literal {
    IntLiteral { value: i64 },
}
