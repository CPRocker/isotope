#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atom(Atom),
    Cons(Op, Vec<Expr>),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Atom(Atom::Boolean(b)) => write!(f, "{}", b),
            Expr::Atom(Atom::Identifier(s)) => write!(f, "{}", s),
            Expr::Atom(Atom::Number(n)) => write!(f, "{}", n),
            Expr::Atom(Atom::String(s)) => write!(f, "\"{}\"", s),
            Expr::Cons(op, args) if op == &Op::Call => {
                for (i, arg) in args.iter().enumerate() {
                    if i == 0 {
                        write!(f, "({}(", arg)?;
                    } else if i == 1 {
                        write!(f, "{}", arg)?;
                    } else {
                        write!(f, ", {}", arg)?;
                    }
                }
                write!(f, "))")
            }
            Expr::Cons(op, args) => {
                write!(f, "({}", op)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Boolean(bool),
    Identifier(String),
    Number(f64),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Call,
    Div,
    Eq,
    Field,
    Gt,
    Gte,
    Index,
    Lt,
    Lte,
    Mod,
    Mul,
    Neg,
    Neq,
    Not,
    Pos,
    Pow,
    Sub,
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Call => write!(f, "()"),
            Op::Div => write!(f, "/"),
            Op::Eq => write!(f, "=="),
            Op::Field => write!(f, "."),
            Op::Gt => write!(f, ">"),
            Op::Gte => write!(f, ">="),
            Op::Index => write!(f, "[]"),
            Op::Lt => write!(f, "<"),
            Op::Lte => write!(f, "<="),
            Op::Mod => write!(f, "%"),
            Op::Mul => write!(f, "*"),
            Op::Neg => write!(f, "-"),
            Op::Neq => write!(f, "!="),
            Op::Not => write!(f, "!"),
            Op::Pos => write!(f, "+"),
            Op::Pow => write!(f, "^"),
            Op::Sub => write!(f, "-"),
        }
    }
}
