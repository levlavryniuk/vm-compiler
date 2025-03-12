use crate::codegen::{Bytecode, BytecodeOp};

pub struct VM {
    constants: Vec<f64>,
    variables: Vec<f64>,
    stack: Vec<f64>,
    ip: usize, // Instruction pointer
}

impl VM {
    pub fn new(bytecode: &Bytecode) -> Self {
        VM {
            constants: bytecode.constants.clone(),
            variables: vec![0.0; bytecode.variables.len()],
            stack: Vec::with_capacity(256),
            ip: 0,
        }
    }

    pub fn execute(&mut self, bytecode: &Bytecode) -> Result<f64, String> {
        while self.ip < bytecode.instructions.len() {
            let (op, operand) = bytecode.instructions[self.ip];

            match op {
                BytecodeOp::LoadConst => {
                    let idx = operand.unwrap();
                    self.stack.push(self.constants[idx]);
                }
                BytecodeOp::LoadVar => {
                    let idx = operand.unwrap();
                    self.stack.push(self.variables[idx]);
                }
                BytecodeOp::StoreVar => {
                    let idx = operand.unwrap();
                    let value = self.stack.pop().unwrap();
                    self.variables[idx] = value;
                }
                BytecodeOp::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a + b);
                }
                BytecodeOp::Subtract => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a - b);
                }
                BytecodeOp::Multiply => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a * b);
                }
                BytecodeOp::Divide => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    if b == 0.0 {
                        return Err("Division by zero".to_string());
                    }

                    self.stack.push(a / b);
                }
                BytecodeOp::Negate => {
                    let a = self.stack.pop().unwrap();
                    self.stack.push(-a);
                }
                BytecodeOp::Return => {
                    // Return the value on top of the stack
                    return if self.stack.is_empty() {
                        Ok(0.0)
                    } else {
                        Ok(self.stack.pop().unwrap())
                    };
                }
            }

            self.ip += 1;
        }

        // If we reach here, there was no return instruction
        if self.stack.is_empty() {
            Ok(0.0)
        } else {
            Ok(self.stack.pop().unwrap())
        }
    }
}
