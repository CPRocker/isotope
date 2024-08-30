use std::collections::HashMap;

use crate::error::CodeGenerationError;

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
