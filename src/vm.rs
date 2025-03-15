use crate::ast_node::LiteralValue;
use crate::codegen::{Bytecode, BytecodeOp};
use crate::{debug, error, info, trace};

pub struct VM {
    constants: Vec<LiteralValue>,
    variables: Vec<LiteralValue>,
    stack: Vec<LiteralValue>,
    ip: usize,
    sp: usize,
    fp: usize,
}

pub struct StackFrame {
    return_addr: usize,
    previus_frame_ptr: usize,
    sp: usize,
    fp: usize,
}
impl VM {
    pub fn new(bytecode: &Bytecode) -> Self {
        info!("VM", "Creating new VM instance");
        VM {
            constants: bytecode.constants.clone(),
            variables: vec![LiteralValue::Number(0.); bytecode.variables.len()],
            stack: Vec::with_capacity(256),
            ip: 0,
            fp: 0,
            sp: 0,
        }
    }

    fn format_instruction(&self, bytecode: &Bytecode) -> String {
        if self.ip >= bytecode.instructions.len() {
            return "END".to_string();
        }

        let (op, operand) = bytecode.instructions[self.ip];
        match operand {
            Some(val) => format!("{:?}({})", op, val),
            None => format!("{:?}", op),
        }
    }

    pub fn execute(&mut self, bytecode: &Bytecode) -> Result<LiteralValue, String> {
        info!("VM", "Starting bytecode execution");

        while self.ip < bytecode.instructions.len() {
            trace!("VM", "Stack: {:?}", &self.stack);
            trace!(
                "VM",
                "IP: {}, Instruction: {}",
                self.ip,
                self.format_instruction(bytecode)
            );

            let (op, operand) = bytecode.instructions[self.ip];

            match op {
                BytecodeOp::LoadConst => {
                    let idx = operand.unwrap();
                    debug!("VM", "Loading constant[{}]: {:?}", idx, self.constants[idx]);
                    self.stack.push(self.constants[idx].clone());
                }
                BytecodeOp::Echo => {
                    let value = self.stack.pop().unwrap();
                    println!("{value}")
                }
                BytecodeOp::LoadVar => {
                    let idx = operand.unwrap();
                    debug!("VM", "Loading variable[{}]: {:?}", idx, self.variables[idx]);
                    self.stack.push(self.variables[idx].clone());
                }
                BytecodeOp::StoreVar => {
                    let idx = operand.unwrap();
                    let value = self.stack.pop().unwrap();
                    debug!("VM", "Storing {:?} in variable[{}]", value, idx);
                    self.variables[idx] = value;
                }
                BytecodeOp::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    debug!("VM", "Add: {:?} + {:?}", a, b);

                    match (a, b) {
                        (LiteralValue::Number(a), LiteralValue::Number(b)) => {
                            let result = a + b;
                            debug!("VM", "Add result: {}", result);
                            self.stack.push(LiteralValue::Number(result))
                        }
                        _ => {
                            error!("VM", "Type error: Cannot add non-numeric values");
                        }
                    }
                }
                BytecodeOp::Subtract => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    debug!("VM", "Subtract: {:?} - {:?}", a, b);

                    match (a, b) {
                        (LiteralValue::Number(a), LiteralValue::Number(b)) => {
                            let result = a - b;
                            debug!("VM", "Subtract result: {}", result);
                            self.stack.push(LiteralValue::Number(result))
                        }
                        _ => {
                            error!("VM", "Type error: Cannot subtract non-numeric values");
                        }
                    }
                }
                BytecodeOp::Multiply => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    debug!("VM", "Multiply: {:?} * {:?}", a, b);

                    match (a, b) {
                        (LiteralValue::Number(a), LiteralValue::Number(b)) => {
                            let result = a * b;
                            debug!("VM", "Multiply result: {}", result);
                            self.stack.push(LiteralValue::Number(result))
                        }
                        _ => {
                            error!("VM", "Type error: Cannot multiply non-numeric values");
                        }
                    }
                }
                BytecodeOp::Divide => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    debug!("VM", "Divide: {:?} / {:?}", a, b);

                    if b == LiteralValue::Number(0.0) {
                        error!("VM", "Runtime error: Division by zero");
                        return Err("Division by zero".to_string());
                    }

                    match (a, b) {
                        (LiteralValue::Number(a), LiteralValue::Number(b)) => {
                            let result = a / b;
                            debug!("VM", "Divide result: {}", result);
                            self.stack.push(LiteralValue::Number(result))
                        }
                        _ => {
                            error!("VM", "Type error: Cannot divide non-numeric values");
                        }
                    }
                }
                BytecodeOp::Negate => {
                    let a = self.stack.pop().unwrap();
                    debug!("VM", "Negate: {:?}", a);

                    if let LiteralValue::Number(a) = a {
                        let result = -a;
                        debug!("VM", "Negate result: {}", result);
                        self.stack.push(LiteralValue::Number(result));
                    } else {
                        error!("VM", "Type error: Cannot negate non-numeric value");
                    }
                }
                BytecodeOp::Return => {
                    let result = if self.stack.is_empty() {
                        debug!("VM", "Return with empty stack, defaulting to 0");
                        LiteralValue::Number(0.)
                    } else {
                        let value = self.stack.pop().unwrap();
                        debug!("VM", "Return value: {:?}", value);
                        value
                    };

                    info!("VM", "Execution completed with result: {:?}", result);
                    return Ok(result);
                }
                BytecodeOp::Jump => {
                    let target = operand.unwrap();
                    debug!("VM", "Jump from {} to {}", self.ip, target);
                    self.ip = target;
                    continue; // Skip the ip increment at the end of the loop
                }
                BytecodeOp::Equals => {
                    let right_val = self.stack.pop().unwrap();
                    let left_val = self.stack.pop().unwrap();
                    debug!("VM", "Equals: {:?} == {:?}", left_val, right_val);

                    let result = left_val == right_val;
                    debug!("VM", "Equals result: {}", result);
                    self.stack.push(LiteralValue::Bool(result));
                }
                BytecodeOp::NotEquals => {
                    let right_val = self.stack.pop().unwrap();
                    let left_val = self.stack.pop().unwrap();
                    debug!("VM", "NotEquals: {:?} != {:?}", left_val, right_val);

                    let result = left_val != right_val;
                    debug!("VM", "NotEquals result: {}", result);
                    self.stack.push(LiteralValue::Bool(result));
                }
                BytecodeOp::Greater => {
                    let right_val = self.stack.pop().unwrap();
                    let left_val = self.stack.pop().unwrap();
                    debug!("VM", "Greater: {:?} > {:?}", left_val, right_val);

                    match left_val > right_val {
                        true => {
                            debug!("VM", "Greater result: true");
                            self.stack.push(LiteralValue::Bool(true));
                        }
                        false => {
                            debug!("VM", "Greater result: false");
                            self.stack.push(LiteralValue::Bool(false));
                        }
                    }
                }
                BytecodeOp::Less => {
                    let right_val = self.stack.pop().unwrap();
                    let left_val = self.stack.pop().unwrap();
                    debug!("VM", "Less: {:?} < {:?}", left_val, right_val);

                    match left_val < right_val {
                        true => {
                            debug!("VM", "Less result: true");
                            self.stack.push(LiteralValue::Bool(true));
                        }
                        false => {
                            debug!("VM", "Less result: false");
                            self.stack.push(LiteralValue::Bool(false));
                        }
                    }
                }
                BytecodeOp::GreaterOrEq => {
                    let right_val = self.stack.pop().unwrap();
                    let left_val = self.stack.pop().unwrap();
                    debug!("VM", "GreaterOrEq: {:?} >= {:?}", left_val, right_val);

                    match left_val >= right_val {
                        true => {
                            debug!("VM", "GreaterOrEq result: true");
                            self.stack.push(LiteralValue::Bool(true));
                        }
                        false => {
                            debug!("VM", "GreaterOrEq result: false");
                            self.stack.push(LiteralValue::Bool(false));
                        }
                    }
                }
                BytecodeOp::LessOrEq => {
                    let right_val = self.stack.pop().unwrap();
                    let left_val = self.stack.pop().unwrap();
                    debug!("VM", "LessOrEq: {:?} <= {:?}", left_val, right_val);

                    match left_val <= right_val {
                        true => {
                            debug!("VM", "LessOrEq result: true");
                            self.stack.push(LiteralValue::Bool(true));
                        }
                        false => {
                            debug!("VM", "LessOrEq result: false");
                            self.stack.push(LiteralValue::Bool(false));
                        }
                    }
                }
                BytecodeOp::JumpIfFalse => {
                    let cond = self.stack.pop().unwrap();
                    let target = operand.unwrap();

                    if let LiteralValue::Bool(condition) = cond {
                        debug!(
                            "VM",
                            "JumpIfFalse condition: {}, target: {}", condition, target
                        );

                        if !condition {
                            debug!("VM", "Condition is false, jumping to {}", target);
                            self.ip = target;
                            continue; // Skip the ip increment at the end of the loop
                        } else {
                            debug!("VM", "Condition is true, not jumping");
                        }
                    } else {
                        error!(
                            "VM",
                            "Type error: JumpIfFalse expected Bool, got {:?}", cond
                        );
                    }
                }
            }

            self.ip += 1;
        }

        // If we reach here, there was no return instruction
        let result = if self.stack.is_empty() {
            debug!(
                "VM",
                "Execution completed with empty stack, defaulting to 0"
            );
            LiteralValue::Number(0.)
        } else {
            let value = self.stack.pop().unwrap();
            debug!(
                "VM",
                "Execution completed, returning top of stack: {:?}", value
            );
            value
        };

        info!("VM", "Execution completed with result: {:?}", result);
        Ok(result)
    }
}
