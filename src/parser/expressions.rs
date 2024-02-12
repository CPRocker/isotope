#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
}

#[derive(Debug)]
pub enum Literal {
    IntLiteral { value: i64 },
}
