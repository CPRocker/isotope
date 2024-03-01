use super::expressions;

#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
}

impl IntoIterator for Program {
    type Item = Statement;

    type IntoIter = std::vec::IntoIter<Statement>;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}

#[derive(Debug)]
pub enum Statement {
    Return {
        expression: expressions::Expression,
    },
    VariableDeclaration {
        identifier: String,
        expression: expressions::Expression,
    },
}
