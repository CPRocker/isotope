use std::borrow::Cow;

use super::Expression;

pub type Name<'iso> = Cow<'iso, str>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'iso> {
    Assignment {
        name: Name<'iso>,
        value: Expression<'iso>,
    },
    Break,
    Expression(Expression<'iso>),
    FunctionDeclaration {
        name: Name<'iso>,
        params: Vec<Name<'iso>>,
        body: Vec<Statement<'iso>>,
    },
    If {
        condition: Expression<'iso>,
        body: Vec<Statement<'iso>>,
        else_body: Option<Vec<Statement<'iso>>>,
    },
    LetDeclaration {
        name: Name<'iso>,
        value: Expression<'iso>,
    },
    Loop {
        body: Vec<Statement<'iso>>,
    },
    Nop,
    Return(Expression<'iso>),
}

impl std::fmt::Display for Statement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Assignment { name, value } => write!(f, "{} = {};", name, value),
            Statement::Break => write!(f, "break;"),
            Statement::Expression(expr) => write!(f, "{}", expr),
            Statement::FunctionDeclaration { name, params, body } => {
                write!(f, "fn {}(", name)?;
                for (i, param) in params.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") {{ ")?;
                for statement in body {
                    write!(f, "{} ", statement)?;
                }
                write!(f, "}}")
            }
            Statement::If {
                condition,
                body,
                else_body,
            } => {
                write!(f, "if({}) {{ ", condition)?;
                for statement in body {
                    write!(f, "{} ", statement)?;
                }
                write!(f, "}}")?;
                if let Some(else_body) = else_body {
                    write!(f, " else {{ ")?;
                    for statement in else_body {
                        write!(f, "{} ", statement)?;
                    }
                    write!(f, "}}")?;
                }
                Ok(())
            }
            Statement::LetDeclaration { name, value } => write!(f, "let {} = {};", name, value),
            Statement::Loop { body } => {
                write!(f, "loop {{ ")?;
                for statement in body {
                    write!(f, "{} ", statement)?;
                }
                write!(f, "}}")
            }
            Statement::Nop => write!(f, "nop;"),
            Statement::Return(expr) => write!(f, "return {};", expr),
        }
    }
}
