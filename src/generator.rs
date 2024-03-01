use std::collections::HashMap;

use crate::{
    error::{self, CodeGenerationError},
    parser::{
        expressions::{BinaryExpression, BinaryOperator, Expression, Literal},
        statements::{Program, Statement},
    },
};

#[derive(Debug)]
struct StackFrame {
    stack_pointer: usize,
    variable_locations: HashMap<String, usize>,
}

impl StackFrame {
    pub fn new() -> Self {
        Self {
            stack_pointer: 0,
            variable_locations: HashMap::new(),
        }
    }

    pub fn size(&self) -> usize {
        if let Some(max_offset) = self.variable_locations.values().max() {
            max_offset + 8
        } else {
            0
        }
    }

    pub fn allocate_variable(&mut self, identifier: String) {
        let variable_size = 8;
        self.variable_locations
            .insert(identifier, self.stack_pointer - variable_size);
    }

    pub fn get_variable_offset(&self, identifier: String) -> Option<usize> {
        self.variable_locations
            .get(&identifier)
            .map(|offset| self.stack_pointer - offset - 8)
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

pub fn generate(program: Program) -> Result<String, error::CodeGenerationError> {
    let mut output = String::from("section .text\n    global main\n\n; program starts\nmain:\n");
    let mut stack_frame = StackFrame::new();

    for statement in program {
        output.push_str(generate_statement(&mut stack_frame, statement)?.as_str());
        output.push('\n');
    }

    Ok(output)
}

fn generate_statement(
    stack_frame: &mut StackFrame,
    statement: Statement,
) -> Result<String, CodeGenerationError> {
    match statement {
        Statement::Return { expression } => generate_return(stack_frame, expression),
        Statement::VariableDeclaration {
            identifier,
            expression,
        } => generate_variable_declaration(stack_frame, identifier, expression),
    }
}

fn generate_return(
    stack_frame: &mut StackFrame,
    expression: Expression,
) -> Result<String, CodeGenerationError> {
    let mut output = String::from("    ; return\n");

    output.push_str(generate_expression(stack_frame, expression)?.as_str());
    output.push_str("    pop rax\n");
    stack_frame.pop(8);
    output.push_str(&format!("    add rsp, {}\n", stack_frame.size()));
    stack_frame.clear();
    output.push_str("    ret\n");

    Ok(output)
}

fn generate_variable_declaration(
    stack_frame: &mut StackFrame,
    identifier: String,
    expression: Expression,
) -> Result<String, CodeGenerationError> {
    let mut output = format!("    ; declare var {}\n", identifier);

    output.push_str(generate_expression(stack_frame, expression)?.as_str());
    stack_frame.allocate_variable(identifier);

    Ok(output)
}

fn generate_expression(
    stack_frame: &mut StackFrame,
    expression: Expression,
) -> Result<String, CodeGenerationError> {
    match expression {
        Expression::Binary(binary_expression) => {
            let mut output = String::new();

            match binary_expression {
                BinaryExpression::Additive(left, op, right) => match op {
                    BinaryOperator::Add => {
                        output.push_str(&generate_expression(stack_frame, *left)?);
                        output.push_str(&generate_expression(stack_frame, *right)?);
                        output.push_str("    pop rbx\n");
                        stack_frame.pop(8);
                        output.push_str("    pop rax\n");
                        stack_frame.pop(8);
                        output.push_str("    add rax, rbx\n");
                        output.push_str("    push rax\n");
                        stack_frame.push(8);
                    }
                    BinaryOperator::Sub => {
                        output.push_str(&generate_expression(stack_frame, *left)?);
                        output.push_str(&generate_expression(stack_frame, *right)?);
                        output.push_str("    pop rbx\n");
                        stack_frame.pop(8);
                        output.push_str("    pop rax\n");
                        stack_frame.pop(8);
                        output.push_str("    sub rax, rbx\n");
                        output.push_str("    push rax\n");
                        stack_frame.push(8);
                    }
                    _ => unreachable!(),
                },
                BinaryExpression::Multiplicative(left, op, right) => match op {
                    BinaryOperator::Mul => {
                        output.push_str(&generate_expression(stack_frame, *left)?);
                        output.push_str(&generate_expression(stack_frame, *right)?);
                        output.push_str("    pop rbx\n");
                        stack_frame.pop(8);
                        output.push_str("    pop rax\n");
                        stack_frame.pop(8);
                        output.push_str("    imul rax, rbx\n");
                        output.push_str("    push rax\n");
                        stack_frame.push(8);
                    }
                    BinaryOperator::Div => {
                        output.push_str(&generate_expression(stack_frame, *left)?);
                        output.push_str(&generate_expression(stack_frame, *right)?);
                        output.push_str("    pop rbx\n");
                        stack_frame.pop(8);
                        output.push_str("    pop rax\n");
                        stack_frame.pop(8);
                        output.push_str("    xor rdx, rdx\n");
                        output.push_str("    idiv rbx\n");
                        output.push_str("    push rax\n");
                        stack_frame.push(8);
                    }
                    _ => unreachable!(),
                },
            };

            Ok(output)
        }
        Expression::Identifier(identifier) => {
            let mut output = format!("    ; get {}\n", identifier);

            if let Some(offset) = stack_frame.get_variable_offset(identifier) {
                output.push_str(&format!("    mov rax, [rsp + {}]\n", offset));
                output.push_str("    push rax\n");
                stack_frame.push(8);

                Ok(output)
            } else {
                Err(CodeGenerationError::UndeclaredVariable)
            }
        }
        Expression::Literal(literal_type) => match literal_type {
            Literal::IntLiteral { value } => {
                let output = format!("    push qword {}\n", value);
                stack_frame.push(8);
                Ok(output)
            }
        },
    }
}
