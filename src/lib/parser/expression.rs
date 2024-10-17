use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'iso> {
    Atom(Atom<'iso>),
    Cons(Op, Vec<Expression<'iso>>),
}

impl std::fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Atom(Atom::Boolean(b)) => write!(f, "{}", b),
            Expression::Atom(Atom::Identifier(s)) => write!(f, "{}", s),
            Expression::Atom(Atom::Number(n)) => write!(f, "{}", n),
            Expression::Atom(Atom::String(s)) => write!(f, "\"{}\"", s),
            Expression::Cons(op, args) if op == &Op::Call => {
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
            Expression::Cons(op, args) => {
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
pub enum Atom<'iso> {
    Boolean(bool),
    Identifier(Cow<'iso, str>),
    Number(f64),
    String(Cow<'iso, str>),
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
