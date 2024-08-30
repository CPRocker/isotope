use crate::{error, parser};

use error::CodeGenerationError;

use self::program::Program;

mod program;

pub fn generate(ast: parser::statements::Program) -> Result<String, error::CodeGenerationError> {
    let mut program = Program::new();
    program.append("section .text\n    global main\n\n; program starts\nmain:\n");

    for statement in ast {
        program = generate_statement(program, statement)?;
        program.append("\n");
    }

    Ok(program.output())
}

fn generate_statement(
    program: Program,
    statement: parser::statements::Statement,
) -> Result<program::Program, error::CodeGenerationError> {
    match statement {
        parser::statements::Statement::Return { expression } => {
            generate_return(program, expression)
        }
        parser::statements::Statement::VariableDeclaration {
            identifier,
            expression,
        } => generate_variable_declaration(program, identifier, expression),
    }
}

fn generate_return(
    mut program: Program,
    expression: parser::expressions::Expression,
) -> Result<program::Program, error::CodeGenerationError> {
    program.append("    ; return\n");

    program = generate_expression(program, expression)?;
    program.pop("rax");
    program.end_scope();
    program.append("    ret\n");

    Ok(program)
}

fn generate_variable_declaration(
    mut program: Program,
    identifier: String,
    expression: parser::expressions::Expression,
) -> Result<Program, CodeGenerationError> {
    program.append(format!("    ; declare var {}\n", identifier).as_str());

    program = generate_expression(program, expression)?;
    program.allocate_variable(identifier.as_str());

    Ok(program)
}

fn generate_expression(
    mut program: Program,
    expression: parser::expressions::Expression,
) -> Result<Program, CodeGenerationError> {
    match expression {
        parser::expressions::Expression::Binary(binary_expression) => {
            match binary_expression {
                parser::expressions::BinaryExpression::Additive(left, op, right) => match op {
                    parser::expressions::BinaryOperator::Add => {
                        program = generate_expression(program, *left)?;
                        program = generate_expression(program, *right)?;
                        program.pop("rbx");
                        program.pop("rax");
                        program.append("    add rax, rbx\n");
                        program.push("rax");
                    }
                    parser::expressions::BinaryOperator::Sub => {
                        program = generate_expression(program, *left)?;
                        program = generate_expression(program, *right)?;
                        program.pop("rbx");
                        program.pop("rax");
                        program.append("    sub rax, rbx\n");
                        program.push("rax");
                    }
                    _ => unreachable!(),
                },
                parser::expressions::BinaryExpression::Multiplicative(left, op, right) => {
                    match op {
                        parser::expressions::BinaryOperator::Mul => {
                            program = generate_expression(program, *left)?;
                            program = generate_expression(program, *right)?;
                            program.pop("rbx");
                            program.pop("rax");
                            program.append("    imul rax, rbx\n");
                            program.push("rax");
                        }
                        parser::expressions::BinaryOperator::Div => {
                            program = generate_expression(program, *left)?;
                            program = generate_expression(program, *right)?;
                            program.pop("rbx");
                            program.pop("rax");
                            program.append("    xor rdx, rdx\n");
                            program.append("    idiv rbx\n");
                            program.push("rax");
                        }
                        _ => unreachable!(),
                    }
                }
            };

            Ok(program)
        }
        parser::expressions::Expression::Identifier(identifier) => {
            program.append(format!("    ; get {}\n", identifier).as_str());

            program.get_variable(identifier.as_str())?;

            Ok(program)
        }
        parser::expressions::Expression::Literal(literal_type) => match literal_type {
            parser::expressions::Literal::IntLiteral { value } => {
                program.push_literal(&value.to_string());

                Ok(program)
            }
        },
    }
}
