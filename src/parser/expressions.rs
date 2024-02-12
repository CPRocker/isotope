#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub enum Literal {
    IntLiteral { value: i64 },
}
