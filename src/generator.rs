use std::collections::HashMap;

use crate::{
    error::{self, CodeGenerationError},
    parser::{
        self,
        expressions::{BinaryExpression, BinaryOperator, Expression, Literal},
        statements::Statement,
    },
};

#[derive(Debug)]
struct Program {
    output: String,
    stack_frame: StackFrame,
}

impl Program {
    const REGISTER_SIZE: usize = 8;

    pub fn new() -> Self {
        Self {
            output: String::new(),
            stack_frame: StackFrame::new(),
        }
    }

    pub fn append(&mut self, string: &str) {
        self.output.push_str(string);
    }

    pub fn output(&self) -> String {
        self.output.clone()
    }

    pub fn end_scope(&mut self) {
        self.append(&format!("    add rsp, {}\n", self.stack_frame.size()));
        self.stack_frame.clear();
    }

    pub fn push(&mut self, register: &str) {
        self.append(&format!("    push {}\n", register));
        self.stack_frame.push(Self::REGISTER_SIZE);
    }

    pub fn push_literal(&mut self, value: &str) {
        self.append(&format!("    push qword {}\n", value));
        self.stack_frame.push(Self::REGISTER_SIZE);
    }

    pub fn pop(&mut self, register: &str) {
        self.append(&format!("    pop {}\n", register));
        self.stack_frame.pop(Self::REGISTER_SIZE);
    }
}

#[derive(Debug)]
struct StackFrame {
    stack_pointer: usize,
    variable_locations: HashMap<String, usize>,
}

impl StackFrame {
    const VARIABLE_SIZE: usize = 8;

    pub fn new() -> Self {
        Self {
            stack_pointer: 0,
            variable_locations: HashMap::new(),
        }
    }

    pub fn size(&self) -> usize {
        self.stack_pointer
    }

    pub fn allocate_variable(&mut self, identifier: String) {
        self.variable_locations
            .insert(identifier, self.stack_pointer - Self::VARIABLE_SIZE);
    }

    pub fn get_variable_offset(&self, identifier: String) -> Option<usize> {
        self.variable_locations
            .get(&identifier)
            .map(|offset| self.stack_pointer - offset - Self::VARIABLE_SIZE)
    }

    pub fn push(&mut self, size: usize) {
        self.stack_pointer += size;
    }

    pub fn pop(&mut self, size: usize) {
        self.stack_pointer -= size;
    }

    pub fn clear(&mut self) {
        self.stack_pointer = 0;
        self.variable_locations = HashMap::new();
    }
}

pub fn generate(
    program: parser::statements::Program,
) -> Result<String, error::CodeGenerationError> {
    let mut output_program = Program::new();
    output_program.append("section .text\n    global main\n\n; program starts\nmain:\n");

    for statement in program {
        output_program = generate_statement(output_program, statement)?;
        output_program.append("\n");
    }

    Ok(output_program.output())
}

fn generate_statement(
    program: Program,
    statement: Statement,
) -> Result<Program, CodeGenerationError> {
    match statement {
        Statement::Return { expression } => generate_return(program, expression),
        Statement::VariableDeclaration {
            identifier,
            expression,
        } => generate_variable_declaration(program, identifier, expression),
    }
}

fn generate_return(
    mut program: Program,
    expression: Expression,
) -> Result<Program, CodeGenerationError> {
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
    expression: Expression,
) -> Result<Program, CodeGenerationError> {
    program.append(&format!("    ; declare var {}\n", identifier));

    program = generate_expression(program, expression)?;
    program.stack_frame.allocate_variable(identifier);

    Ok(program)
}

fn generate_expression(
    mut program: Program,
    expression: Expression,
) -> Result<Program, CodeGenerationError> {
    match expression {
        Expression::Binary(binary_expression) => {
            match binary_expression {
                BinaryExpression::Additive(left, op, right) => match op {
                    BinaryOperator::Add => {
                        program = generate_expression(program, *left)?;
                        program = generate_expression(program, *right)?;
                        program.pop("rbx");
                        program.pop("rax");
                        program.append("    add rax, rbx\n");
                        program.push("rax");
                    }
                    BinaryOperator::Sub => {
                        program = generate_expression(program, *left)?;
                        program = generate_expression(program, *right)?;
                        program.pop("rbx");
                        program.pop("rax");
                        program.append("    sub rax, rbx\n");
                        program.push("rax");
                    }
                    _ => unreachable!(),
                },
                BinaryExpression::Multiplicative(left, op, right) => match op {
                    BinaryOperator::Mul => {
                        program = generate_expression(program, *left)?;
                        program = generate_expression(program, *right)?;
                        program.pop("rbx");
                        program.pop("rax");
                        program.append("    imul rax, rbx\n");
                        program.push("rax");
                    }
                    BinaryOperator::Div => {
                        program = generate_expression(program, *left)?;
                        program = generate_expression(program, *right)?;
                        program.pop("rbx");
                        program.pop("rax");
                        program.append("    xor rdx, rdx\n");
                        program.append("    idiv rbx\n");
                        program.push("rax");
                    }
                    _ => unreachable!(),
                },
            };

            Ok(program)
        }
        Expression::Identifier(identifier) => {
            program.append(&format!("    ; get {}\n", identifier));

            if let Some(offset) = program.stack_frame.get_variable_offset(identifier) {
                program.append(&format!("    mov rax, [rsp + {}]\n", offset));
                program.push("rax");

                Ok(program)
            } else {
                Err(CodeGenerationError::UndeclaredVariable)
            }
        }
        Expression::Literal(literal_type) => match literal_type {
            Literal::IntLiteral { value } => {
                program.push_literal(&value.to_string());

                Ok(program)
            }
        },
    }
}
