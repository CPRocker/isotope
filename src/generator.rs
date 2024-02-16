use crate::parser::{
    expressions::{Expression, Literal},
    statements::{Program, Statement},
};

pub fn generate(program: Program) -> String {
    let mut output = String::from("section .text\n    global main\n\n; program starts\nmain:\n");

    for statement in program {
        output.push_str(generate_statement(statement).as_str());
        output.push('\n');
    }

    output
}

fn generate_statement(statement: Statement) -> String {
    match statement {
        Statement::Return { expression } => generate_return(expression),
    }
}

fn generate_return(expression: Expression) -> String {
    let mut output = String::from("    ; return\n");

    output.push_str(generate_expression(expression).as_str());
    output.push_str("    pop rax\n");
    output.push_str("    ret\n");

    output
}

fn generate_expression(expression: Expression) -> String {
    match expression {
        Expression::Literal(literal_type) => match literal_type {
            Literal::IntLiteral { value } => format!("    push dword {}\n", value),
        },
    }
}
