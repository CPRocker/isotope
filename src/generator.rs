use crate::{error, parser};

use error::CodeGenerationError;

use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
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

    pub fn allocate_variable(&mut self, identifier: &str) {
        self.stack_frame.allocate_variable(identifier);
    }

    pub fn get_variable(&mut self, identifier: &str) -> Result<(), CodeGenerationError> {
        if let Some(offset) = self.stack_frame.get_variable_offset(identifier) {
            self.append(format!("    mov rax, [rsp + {}]\n", offset).as_str());
            self.push("rax");

            Ok(())
        } else {
            Err(CodeGenerationError::UndeclaredVariable)
        }
    }

    pub fn open_scope(&mut self) {}

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

    pub fn allocate_variable(&mut self, identifier: &str) {
        self.variable_locations.insert(
            identifier.to_string(),
            self.stack_pointer - Self::VARIABLE_SIZE,
        );
    }

    pub fn get_variable_offset(&self, identifier: &str) -> Option<usize> {
        self.variable_locations
            .get(identifier)
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

pub fn generate(ast: parser::Program) -> Result<String, error::CodeGenerationError> {
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
    statement: parser::Statement,
) -> Result<Program, error::CodeGenerationError> {
    match statement {
        parser::Statement::Return { expression } => generate_return(program, expression),
        parser::Statement::VariableDeclaration {
            identifier,
            expression,
        } => generate_variable_declaration(program, identifier, expression),
    }
}

fn generate_return(
    mut program: Program,
    expression: parser::Expression,
) -> Result<Program, error::CodeGenerationError> {
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
    expression: parser::Expression,
) -> Result<Program, CodeGenerationError> {
    program.append(format!("    ; declare var {}\n", identifier).as_str());

    program = generate_expression(program, expression)?;
    program.allocate_variable(identifier.as_str());

    Ok(program)
}

fn generate_expression(
    mut program: Program,
    expression: parser::Expression,
) -> Result<Program, CodeGenerationError> {
    match expression {
        parser::Expression::Binary(binary_expression) => {
            match binary_expression {
                parser::BinaryExpression::Additive(left, op, right) => match op {
                    parser::BinaryOperator::Add => {
                        program = generate_expression(program, *left)?;
                        program = generate_expression(program, *right)?;
                        program.pop("rbx");
                        program.pop("rax");
                        program.append("    add rax, rbx\n");
                        program.push("rax");
                    }
                    parser::BinaryOperator::Sub => {
                        program = generate_expression(program, *left)?;
                        program = generate_expression(program, *right)?;
                        program.pop("rbx");
                        program.pop("rax");
                        program.append("    sub rax, rbx\n");
                        program.push("rax");
                    }
                    _ => unreachable!(),
                },
                parser::BinaryExpression::Multiplicative(left, op, right) => match op {
                    parser::BinaryOperator::Mul => {
                        program = generate_expression(program, *left)?;
                        program = generate_expression(program, *right)?;
                        program.pop("rbx");
                        program.pop("rax");
                        program.append("    imul rax, rbx\n");
                        program.push("rax");
                    }
                    parser::BinaryOperator::Div => {
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
        parser::Expression::Identifier(identifier) => {
            program.append(format!("    ; get {}\n", identifier).as_str());

            program.get_variable(identifier.as_str())?;

            Ok(program)
        }
        parser::Expression::Literal(literal_type) => match literal_type {
            parser::Literal::IntLiteral { value } => {
                program.push_literal(&value.to_string());

                Ok(program)
            }
        },
    }
}
