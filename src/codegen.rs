use std::{collections::HashMap, ops::Deref};

use crate::{
    ast_node::LiteralValue,
    debug, error, info,
    ir::{Instruction, IrProgram, OpCode, Operand},
    trace,
};

#[derive(Debug, Clone, Copy)]
pub enum BytecodeOp {
    LoadConst,
    LoadVar,
    StoreVar,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Echo,
    Equals,
    NotEquals,
    Return,

    Greater,
    Less,
    GreaterOrEq,
    Call,      // New: Call a function
    Push,      // New: Push value to stack
    LoadParam, // New: Load parameter from stack
    LessOrEq,
    JumpIfFalse,
    Jump,
}

#[derive(Debug)]
pub struct Bytecode {
    pub constants: Vec<LiteralValue>,
    pub variables: Vec<String>,
    pub functions: HashMap<String, (usize, usize, bool)>,
    // name -> (start_address, param_count, does_return?)
    pub instructions: Vec<(BytecodeOp, Option<usize>)>,
}

pub struct BytecodeGenerator {
    constants: Vec<LiteralValue>,
    pub functions: HashMap<String, (usize, usize, bool)>, // name -> (start_address, param_count)
    variables: Vec<String>,
    instructions: Vec<(BytecodeOp, Option<usize>)>,
}

impl BytecodeGenerator {
    pub fn new() -> Self {
        info!("Bytecode", "Creating new Bytecode generator");
        BytecodeGenerator {
            constants: Vec::new(),
            variables: Vec::new(),
            instructions: Vec::new(),
            functions: HashMap::new(),
        }
    }

    pub fn generate(&mut self, ir: &IrProgram) -> Bytecode {
        info!(
            "Bytecode",
            "Starting Bytecode generation for {} instructions",
            ir.instructions.len()
        );
        for instruction in &ir.instructions {
            trace!(
                "Bytecode",
                "Generating Bytecode for instruction: {:?}",
                instruction
            );
            self.generate_instruction(instruction);
        }

        info!(
            "Bytecode",
            "Bytecode generation complete, produced {} instructions",
            self.instructions.len()
        );
        Bytecode {
            constants: self.constants.clone(),
            variables: self.variables.clone(),
            functions: self.functions.clone(),
            instructions: self.instructions.clone(),
        }
    }

    fn generate_instruction(&mut self, instruction: &Instruction) {
        match instruction.op {
            OpCode::LoadConst => {
                if let Operand::Immediate(value) = &instruction.operands[1] {
                    let const_idx = self.add_constant(value.clone());
                    self.emit(BytecodeOp::LoadConst, Some(const_idx));
                    trace!("Bytecode", "Loaded constant at index {}", const_idx);
                }
            }
            OpCode::FunctionDeclaration => {
                if let (
                    Operand::Immediate(LiteralValue::String(name)),
                    Operand::Immediate(LiteralValue::Number(param_count)),
                    Operand::Immediate(LiteralValue::Bool(has_return_value)),
                ) = (
                    &instruction.operands[0],
                    &instruction.operands[1],
                    &instruction.operands[2],
                ) {
                    // Record the current position as the start of this function
                    let function_start = self.instructions.len();
                    self.functions.insert(
                        name.clone(),
                        (function_start, *param_count as usize, *has_return_value),
                    );
                    info!(
                        "Bytecode",
                        "Registered function '{}' at position {} with {} params",
                        name,
                        function_start,
                        param_count
                    );
                }
            }

            OpCode::Call => {
                if let [Operand::Register(result_reg), Operand::Immediate(LiteralValue::String(name)), Operand::Immediate(LiteralValue::Number(_))] =
                    instruction.operands.as_slice()
                {
                    if let Some((function_addr, .., returns_value)) = self.functions.get(name) {
                        let function_addr = *function_addr;
                        let returns_value = *returns_value;

                        self.emit(BytecodeOp::Call, Some(function_addr));
                        // if returns_value {
                        //     self.emit(BytecodeOp::StoreVar, Some(*result_reg));
                        //     trace!("Bytecode", "Stored return value in register {}", result_reg);
                        // }
                        trace!(
                            "Bytecode",
                            "Generated call to function '{}' at address {}",
                            name,
                            function_addr
                        );
                    } else {
                        error!("Bytecode", "Function not found: {}", name);
                        panic!("Function not found: {}", name);
                    }
                } else {
                    panic!("Invalid operands for Call instruction");
                }
            }

            OpCode::Return => {
                if let Some(Operand::Register(reg_idx)) = instruction.operands.last() {
                    self.emit(BytecodeOp::Return, Some(*reg_idx));
                    trace!(
                        "Bytecode",
                        "Generated return instruction with register {}",
                        reg_idx
                    );
                } else {
                    self.emit(BytecodeOp::Return, None);
                    trace!("Bytecode", "Generated return instruction with no value");
                }
            }

            OpCode::LoadParam => {
                if let Operand::Immediate(LiteralValue::Number(idx)) = &instruction.operands[1] {
                    self.emit(BytecodeOp::LoadParam, Some(*idx as usize));
                    trace!("Bytecode", "Loaded parameter at index {}", idx);
                }
            }

            OpCode::Push => {
                if let Operand::Register(reg) = instruction.operands[0] {
                    self.emit(BytecodeOp::Push, Some(reg));
                    trace!("Bytecode", "Pushed value from register {} to stack", reg);
                }
            }
            OpCode::DeclareVar => {
                if let Operand::Immediate(LiteralValue::String(s)) = &instruction.operands[0] {
                    let var_idx = self.add_variable(s);
                    self.emit(BytecodeOp::StoreVar, Some(var_idx));
                    trace!("Bytecode", "Declared variable '{}' at index {}", s, var_idx);
                }
            }

            OpCode::Equals => {
                self.emit(BytecodeOp::Equals, None);
                trace!("Bytecode", "Generated Equals instruction");
            }

            OpCode::LessOrEq => {
                self.emit(BytecodeOp::LessOrEq, None);
                trace!("Bytecode", "Generated LessOrEq instruction");
            }

            OpCode::Less => {
                self.emit(BytecodeOp::Less, None);
                trace!("Bytecode", "Generated Less instruction");
            }

            OpCode::GreaterOrEq => {
                self.emit(BytecodeOp::GreaterOrEq, None);
                trace!("Bytecode", "Generated GreaterOrEq instruction");
            }

            OpCode::Greater => {
                self.emit(BytecodeOp::Greater, None);
                trace!("Bytecode", "Generated Greater instruction");
            }
            OpCode::NotEquals => {
                self.emit(BytecodeOp::NotEquals, None);
                trace!("Bytecode", "Generated NotEquals instruction");
            }
            OpCode::Echo => {
                self.emit(BytecodeOp::Echo, None);
                trace!("Bytecode", "Generated Echo instruction");
            }

            OpCode::LoadVar => {
                if let Operand::Variable(ref name) = instruction.operands[1] {
                    let var_idx = self.add_variable(name);
                    self.emit(BytecodeOp::LoadVar, Some(var_idx));
                    trace!(
                        "Bytecode",
                        "Loaded variable '{}' at index {}",
                        name,
                        var_idx
                    );
                }
            }
            OpCode::StoreVar => {
                if let Operand::Variable(ref name) = instruction.operands[0] {
                    let var_idx = self.add_variable(name);
                    self.emit(BytecodeOp::StoreVar, Some(var_idx));
                    trace!(
                        "Bytecode",
                        "Stored variable '{}' at index {}",
                        name,
                        var_idx
                    );
                }
            }
            OpCode::Jump => {
                if let Operand::Immediate(LiteralValue::Number(target_addr)) =
                    instruction.operands[0]
                {
                    self.emit(BytecodeOp::Jump, Some(target_addr as usize));
                    trace!(
                        "Bytecode",
                        "Generated Jump instruction to address {}",
                        target_addr
                    );
                }
            }
            OpCode::JumpIfFalse => {
                if let Operand::Immediate(LiteralValue::Number(target_addr)) =
                    instruction.operands[1]
                {
                    self.emit(BytecodeOp::JumpIfFalse, Some(target_addr as usize));
                    trace!(
                        "Bytecode",
                        "Generated JumpIfFalse instruction to address {}",
                        target_addr
                    );
                }
            }
            OpCode::Add => {
                self.emit(BytecodeOp::Add, None);
                trace!("Bytecode", "Generated Add instruction");
            }
            OpCode::Subtract => {
                self.emit(BytecodeOp::Subtract, None);
                trace!("Bytecode", "Generated Subtract instruction");
            }
            OpCode::Multiply => {
                self.emit(BytecodeOp::Multiply, None);
                trace!("Bytecode", "Generated Multiply instruction");
            }
            OpCode::Divide => {
                self.emit(BytecodeOp::Divide, None);
                trace!("Bytecode", "Generated Divide instruction");
            }
            OpCode::Negate => {
                self.emit(BytecodeOp::Negate, None);
                trace!("Bytecode", "Generated Negate instruction");
            }
            _ => (),
        }
    }

    fn add_constant(&mut self, value: LiteralValue) -> usize {
        if let Some(idx) = self.constants.iter().position(|c| *c == value) {
            return idx;
        }

        let idx = self.constants.len();
        self.constants.push(value);
        trace!("Bytecode", "Added constant at index {}", idx);
        idx
    }

    fn add_variable(&mut self, name: &str) -> usize {
        if let Some(idx) = self.variables.iter().position(|v| v == name) {
            return idx;
        }

        let idx = self.variables.len();
        self.variables.push(name.to_string());
        trace!("Bytecode", "Added variable '{}' at index {}", name, idx);
        idx
    }

    fn emit(&mut self, op: BytecodeOp, operand: Option<usize>) {
        self.instructions.push((op, operand));
        trace!(
            "Bytecode",
            "Emitted instruction: {:?} with operand {:?}",
            op,
            operand
        );
    }
}
