use crate::ir::{Instruction, IrProgram, OpCode, Operand};

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
    Return,
}

#[derive(Debug)]
pub struct Bytecode {
    pub constants: Vec<f64>,
    pub variables: Vec<String>,
    pub instructions: Vec<(BytecodeOp, Option<usize>)>,
}

pub struct BytecodeGenerator {
    constants: Vec<f64>,
    variables: Vec<String>,
    instructions: Vec<(BytecodeOp, Option<usize>)>,
}

impl BytecodeGenerator {
    pub fn new() -> Self {
        BytecodeGenerator {
            constants: Vec::new(),
            variables: Vec::new(),
            instructions: Vec::new(),
        }
    }

    pub fn generate(&mut self, ir: &IrProgram) -> Bytecode {
        for instruction in &ir.instructions {
            self.generate_instruction(instruction);
        }

        // Add a return instruction at the end
        self.emit(BytecodeOp::Return, None);

        Bytecode {
            constants: self.constants.clone(),
            variables: self.variables.clone(),
            instructions: self.instructions.clone(),
        }
    }

    fn generate_instruction(&mut self, instruction: &Instruction) {
        match instruction.op {
            OpCode::LoadConst => {
                if let Operand::Immediate(value) = instruction.operands[1] {
                    let const_idx = self.add_constant(value);
                    self.emit(BytecodeOp::LoadConst, Some(const_idx));
                }
            }
            OpCode::LoadVar => {
                if let Operand::Variable(ref name) = instruction.operands[1] {
                    let var_idx = self.add_variable(name);
                    self.emit(BytecodeOp::LoadVar, Some(var_idx));
                }
            }
            OpCode::StoreVar => {
                if let Operand::Variable(ref name) = instruction.operands[0] {
                    let var_idx = self.add_variable(name);
                    self.emit(BytecodeOp::StoreVar, Some(var_idx));
                }
            }
            OpCode::Add => self.emit(BytecodeOp::Add, None),
            OpCode::Subtract => self.emit(BytecodeOp::Subtract, None),
            OpCode::Multiply => self.emit(BytecodeOp::Multiply, None),
            OpCode::Divide => self.emit(BytecodeOp::Divide, None),
            OpCode::Negate => self.emit(BytecodeOp::Negate, None),
        }
    }

    fn add_constant(&mut self, value: f64) -> usize {
        // Check if the constant already exists
        if let Some(idx) = self.constants.iter().position(|&c| c == value) {
            return idx;
        }

        let idx = self.constants.len();
        self.constants.push(value);
        idx
    }

    fn add_variable(&mut self, name: &str) -> usize {
        // Check if the variable already exists
        if let Some(idx) = self.variables.iter().position(|v| v == name) {
            return idx;
        }

        let idx = self.variables.len();
        self.variables.push(name.to_string());
        idx
    }

    fn emit(&mut self, op: BytecodeOp, operand: Option<usize>) {
        self.instructions.push((op, operand));
    }
}
