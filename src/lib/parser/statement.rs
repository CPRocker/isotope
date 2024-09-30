use super::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Assignment {
        name: Identifier,
        value: Expr,
    },
    Break,
    Expr(Expr),
    FunctionDeclaration {
        name: Identifier,
        params: Vec<Identifier>,
        body: Vec<Stmt>,
    },
    If {
        condition: Expr,
        body: Vec<Stmt>,
        else_body: Option<Vec<Stmt>>,
    },
    LetDeclaration {
        name: Identifier,
        value: Expr,
    },
    Loop {
        body: Vec<Stmt>,
    },
    Nop,
    Return(Expr),
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Assignment { name, value } => write!(f, "{} = {};", name, value),
            Stmt::Break => write!(f, "break;"),
            Stmt::Expr(expr) => write!(f, "{}", expr),
            Stmt::FunctionDeclaration { name, params, body } => {
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
            Stmt::If {
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
            Stmt::LetDeclaration { name, value } => write!(f, "let {} = {};", name, value),
            Stmt::Loop { body } => {
                write!(f, "loop {{ ")?;
                for statement in body {
                    write!(f, "{} ", statement)?;
                }
                write!(f, "}}")
            }
            Stmt::Nop => write!(f, "nop;"),
            Stmt::Return(expr) => write!(f, "return {};", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    name: String,
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Self { name }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
