use crate::parser::{
    expressions::{Expression, Literal},
    statements::{Program, Statement},
};

pub fn generate(program: Program) -> String {
    let mut output = String::from("global _start\n\n; program starts\n_start:\n");

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

    output.push_str("    mov eax, 1\n");
    output.push_str(generate_expression(expression).as_str());
    output.push_str("    push eax\n");
    output.push_str("    int 0x80\n");

    output
}

fn generate_expression(expression: Expression) -> String {
    match expression {
        Expression::Literal(literal_type) => match literal_type {
            Literal::IntLiteral { value } => format!("    push dword, {}\n", value),
        },
    }
}
