use crate::ast_node::*;
use crate::tokens::*;
use crate::{debug, error, info, trace};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    LoadConst,
    LoadVar,
    StoreVar,
    DeclareVar,
    Return,

    Add,
    Subtract,
    Multiply,
    Divide,

    Equals,
    NotEquals,
    Greater,
    FunctionDeclaration,
    Less,
    GreaterOrEq,
    LessOrEq,

    Negate,

    Echo,

    Call,      // New: Call a function
    Push,      // New: Push value to stack
    LoadParam, // New: Load parameter from stack
    Jump,
    JumpIfFalse,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub op: OpCode,
    pub operands: Vec<Operand>,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Register(usize),
    Immediate(LiteralValue),
    Variable(String),
}

#[derive(Debug)]
pub struct IrProgram {
    pub instructions: Vec<Instruction>,
    pub functions: HashMap<String, (usize, usize, bool)>, // name -> (start_address, param_count)
}

pub struct IrGenerator {
    instructions: Vec<Instruction>,
    pub functions: HashMap<String, (usize, usize, bool)>, // name -> (start_address, param_count)
    temp_counter: usize,
}

impl IrGenerator {
    pub fn new() -> Self {
        info!("IR", "Creating new IR generator");
        IrGenerator {
            instructions: Vec::new(),
            temp_counter: 0,
            functions: HashMap::new(),
        }
    }

    pub fn generate(&mut self, statements: &[Stmt]) -> IrProgram {
        info!(
            "IR",
            "Starting IR generation for {} statements",
            statements.len()
        );
        for stmt in statements {
            trace!("IR", "Generating IR for statement: {:?}", stmt);
            self.generate_statement(stmt);
        }

        info!(
            "IR",
            "IR generation complete, produced {} instructions and {} functions",
            self.instructions.len(),
            self.functions.len()
        );
        IrProgram {
            instructions: self.instructions.clone(),
            functions: self.functions.clone(),
        }
    }

    fn generate_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression { expression } => {
                debug!("IR", "Generating IR for expression statement");
                self.generate_expression(expression);
                debug!("IR", "Successfully generated IR for expression statement");
            }
            Stmt::Return { value } => {
                debug!("IR", "Generating IR for return statement");
                if let Some(value) = value {
                    trace!("IR", "Return statement has a value expression");
                    let reg = self.generate_expression(value);
                    self.emit(OpCode::Return, vec![Operand::Register(reg)]);
                    debug!("IR", "Generated return with value in register {}", reg);
                } else {
                    trace!("IR", "Return statement with no value");
                    self.emit(OpCode::Return, vec![]);
                    debug!("IR", "Generated return with no value");
                }
            }
            Stmt::FunctionDeclaration {
                args,
                name,
                body,
                returns_value,
            } => {
                debug!("IR", "Generating IR for function declaration '{}'", name);
                let function_start = self.instructions.len();
                trace!(
                    "IR",
                    "Function '{}' starts at instruction index {}",
                    name,
                    function_start
                );

                // Add jump over the function body
                self.emit(
                    OpCode::Jump,
                    vec![Operand::Immediate(LiteralValue::Number(0.))],
                );
                trace!("IR", "Added jump placeholder at start of function");

                // Function declaration
                self.emit(
                    OpCode::FunctionDeclaration,
                    vec![
                        Operand::Immediate(LiteralValue::String(name.clone())),
                        Operand::Immediate(LiteralValue::Number(args.len() as f64)),
                        Operand::Immediate(LiteralValue::Bool(*returns_value)),
                    ],
                );
                debug!(
                    "IR",
                    "Function '{}' has {} args, returns_value: {}",
                    name,
                    args.len(),
                    returns_value
                );

                // Generate function body
                trace!("IR", "Generating IR for function body");
                self.generate_statement(body);
                debug!("IR", "Successfully generated function body");

                // Add implicit return if needed
                self.emit(OpCode::Return, vec![]);
                trace!("IR", "Added implicit return at end of function");

                let function_end = self.instructions.len();
                trace!(
                    "IR",
                    "Function '{}' ends at instruction index {}",
                    name,
                    function_end
                );

                // Fix up the jump at the start of the function
                if let Operand::Immediate(LiteralValue::Number(ref mut value)) =
                    self.instructions[function_start].operands[0]
                {
                    *value = function_end as f64 - 1.0;
                    trace!(
                        "IR",
                        "Updated jump at start of function to {} (end of function)",
                        function_end - 1
                    );
                }

                // Register the function
                self.functions
                    .insert(name.clone(), (function_start, args.len(), *returns_value));
                info!(
                    "IR",
                    "Successfully registered function '{}' at position {} with {} args",
                    name,
                    function_start,
                    args.len()
                );
            }

            Stmt::Echo { expression } => {
                debug!("IR", "Generating IR for echo statement");
                let expr_temp = self.generate_expression(expression);
                debug!("IR", "Echo expression result in register {}", expr_temp);

                self.emit(OpCode::Echo, vec![Operand::Register(expr_temp)]);
                debug!("IR", "Successfully generated IR for echo statement");
            }
            Stmt::Let { name, initializer } => {
                debug!("IR", "Generating IR for let statement");
                if let Some(init) = initializer {
                    trace!("IR", "Let statement has initializer");
                    let temp = self.generate_expression(init);
                    debug!("IR", "Initializer result in register {}", temp);

                    if let TokenType::Identifier(name_str) = &name.token_type {
                        self.emit(
                            OpCode::StoreVar,
                            vec![Operand::Variable(name_str.clone()), Operand::Register(temp)],
                        );
                        debug!(
                            "IR",
                            "Stored variable '{}' with value from register {}", name_str, temp
                        );
                    }
                } else if let TokenType::Identifier(name_str) = &name.token_type {
                    trace!("IR", "Let statement without initializer");
                    self.emit(
                        OpCode::DeclareVar,
                        vec![Operand::Variable(name_str.clone())],
                    );
                    debug!("IR", "Declared variable '{}' without initializer", name_str);
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                debug!("IR", "Generating IR for if statement");
                let condition_temp = self.generate_expression(condition);
                debug!("IR", "If condition result in register {}", condition_temp);

                let jump_if_false_idx = self.instructions.len();
                trace!("IR", "Adding JumpIfFalse at index {}", jump_if_false_idx);

                self.emit(
                    OpCode::JumpIfFalse,
                    vec![
                        Operand::Register(condition_temp),
                        Operand::Immediate(LiteralValue::Number(0.)),
                    ],
                );

                trace!("IR", "Generating IR for then branch");
                self.generate_statement(then_branch);
                debug!("IR", "Successfully generated IR for then branch");

                if let Some(else_branch) = else_branch {
                    debug!("IR", "If statement has else branch");
                    let jump_end_idx = self.instructions.len();
                    trace!(
                        "IR",
                        "Adding jump over else branch at index {}",
                        jump_end_idx
                    );

                    self.emit(
                        OpCode::Jump,
                        vec![Operand::Immediate(LiteralValue::Number(0.0))],
                    );

                    let else_start_idx = self.instructions.len();
                    trace!("IR", "Else branch starts at index {}", else_start_idx);

                    // Update the jump if false to jump to else
                    if let Operand::Immediate(LiteralValue::Number(ref mut value)) =
                        self.instructions[jump_if_false_idx].operands[1]
                    {
                        *value = else_start_idx as f64;
                        trace!(
                            "IR",
                            "Updated JumpIfFalse to jump to else branch at {}",
                            else_start_idx
                        );
                    }

                    trace!("IR", "Generating IR for else branch");
                    self.generate_statement(else_branch);
                    debug!("IR", "Successfully generated IR for else branch");

                    let end_idx = self.instructions.len();
                    trace!("IR", "If-else statement ends at index {}", end_idx);

                    // Update the jump at the end of the then branch
                    if let Operand::Immediate(LiteralValue::Number(ref mut value)) =
                        self.instructions[jump_end_idx].operands[0]
                    {
                        *value = end_idx as f64 - 1.0;
                        trace!(
                            "IR",
                            "Updated jump at end of then branch to {}",
                            end_idx - 1
                        );
                    }

                    debug!("IR", "Successfully generated complete if-else statement");
                } else {
                    debug!("IR", "If statement has no else branch");
                    let end_idx = self.instructions.len();
                    trace!("IR", "If statement ends at index {}", end_idx);

                    // Update the jump if false to jump to the end
                    if let Operand::Immediate(LiteralValue::Number(ref mut value)) =
                        self.instructions[jump_if_false_idx].operands[1]
                    {
                        *value = end_idx as f64 - 1.0;
                        trace!(
                            "IR",
                            "Updated JumpIfFalse to jump to end at {}",
                            end_idx - 1
                        );
                    }

                    debug!("IR", "Successfully generated if statement without else");
                }
            }
            Stmt::Block { statements } => {
                debug!(
                    "IR",
                    "Generating IR for block statement with {} statements",
                    statements.len()
                );
                for st in statements {
                    trace!("IR", "Generating IR for statement in block: {:?}", st);
                    self.generate_statement(st);
                }
                debug!("IR", "Successfully generated IR for block statement");
            }
        }
    }

    fn generate_expression(&mut self, expr: &Expr) -> usize {
        match expr {
            Expr::Call { callee, arguments } => {
                debug!(
                    "IR",
                    "Generating IR for function call '{}' with {} arguments",
                    callee,
                    arguments.len()
                );

                // First, evaluate all arguments and push them onto the stack
                // (in reverse order, so first arg is on top)
                for (i, arg) in arguments.iter().rev().enumerate() {
                    trace!(
                        "IR",
                        "Generating IR for function argument {} (in reverse order)",
                        i
                    );
                    let arg_temp = self.generate_expression(arg);
                    debug!(
                        "IR",
                        "Function argument {} result in register {}", i, arg_temp
                    );

                    self.emit(OpCode::Push, vec![Operand::Register(arg_temp)]);
                    trace!("IR", "Pushed argument from register {} to stack", arg_temp);
                }

                // Generate the call instruction
                let result_temp = self.new_temp();
                trace!(
                    "IR",
                    "Allocating register {} for function call result",
                    result_temp
                );

                self.emit(
                    OpCode::Call,
                    vec![
                        Operand::Register(result_temp),
                        Operand::Immediate(LiteralValue::String(callee.clone())),
                        Operand::Immediate(LiteralValue::Number(arguments.len() as f64)),
                    ],
                );

                debug!(
                    "IR",
                    "Generated call to function '{}' with {} arguments, result in register {}",
                    callee,
                    arguments.len(),
                    result_temp
                );

                result_temp
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                debug!(
                    "IR",
                    "Generating IR for binary expression with operator {:?}", operator.token_type
                );

                let left_temp = self.generate_expression(left);
                trace!("IR", "Left operand result in register {}", left_temp);

                let right_temp = self.generate_expression(right);
                trace!("IR", "Right operand result in register {}", right_temp);

                let result_temp = self.new_temp();
                trace!(
                    "IR",
                    "Allocating register {} for binary operation result",
                    result_temp
                );

                let op = match operator.token_type {
                    TokenType::Plus => OpCode::Add,
                    TokenType::Minus => OpCode::Subtract,
                    TokenType::Star => OpCode::Multiply,
                    TokenType::Slash => OpCode::Divide,
                    TokenType::EqualEqual => OpCode::Equals,
                    TokenType::BangEqual => OpCode::NotEquals,
                    TokenType::Greater => OpCode::Greater,
                    TokenType::GreaterOrEq => OpCode::GreaterOrEq,
                    TokenType::Less => OpCode::Less,
                    TokenType::LessOrEq => OpCode::LessOrEq,
                    _ => {
                        error!(
                            "IR",
                            "Unexpected binary operator: {:?}", operator.token_type
                        );
                        panic!("Unexpected binary operator");
                    }
                };

                self.emit(
                    op.clone(),
                    vec![
                        Operand::Register(result_temp),
                        Operand::Register(left_temp),
                        Operand::Register(right_temp),
                    ],
                );

                debug!(
                    "IR",
                    "Generated binary operation {:?} with registers {} and {}, result in {}",
                    op,
                    left_temp,
                    right_temp,
                    result_temp
                );

                result_temp
            }
            Expr::Unary { operator, right } => {
                debug!(
                    "IR",
                    "Generating IR for unary expression with operator {:?}", operator.token_type
                );

                let right_temp = self.generate_expression(right);
                trace!("IR", "Unary operand result in register {}", right_temp);

                let result_temp = self.new_temp();
                trace!(
                    "IR",
                    "Allocating register {} for unary operation result",
                    result_temp
                );

                match operator.token_type {
                    TokenType::Minus => {
                        self.emit(
                            OpCode::Negate,
                            vec![
                                Operand::Register(result_temp),
                                Operand::Register(right_temp),
                            ],
                        );
                        debug!(
                            "IR",
                            "Generated negate operation on register {}, result in {}",
                            right_temp,
                            result_temp
                        );
                    }
                    _ => {
                        error!("IR", "Unexpected unary operator: {:?}", operator.token_type);
                        panic!("Unexpected unary operator");
                    }
                }

                result_temp
            }
            Expr::Grouping { expression } => {
                debug!("IR", "Generating IR for grouped expression");
                let result = self.generate_expression(expression);
                debug!("IR", "Grouped expression result in register {}", result);
                result
            }
            Expr::Literal { value } => {
                debug!("IR", "Generating IR for literal: {:?}", value);

                let result_temp = self.new_temp();
                trace!(
                    "IR",
                    "Allocating register {} for literal value",
                    result_temp
                );

                match value {
                    LiteralValue::Number(n) => {
                        self.emit(
                            OpCode::LoadConst,
                            vec![
                                Operand::Register(result_temp),
                                Operand::Immediate(value.clone()),
                            ],
                        );
                        debug!(
                            "IR",
                            "Loaded number constant {} into register {}", n, result_temp
                        );
                    }
                    LiteralValue::String(s) => {
                        self.emit(
                            OpCode::LoadConst,
                            vec![
                                Operand::Register(result_temp),
                                Operand::Immediate(value.clone()),
                            ],
                        );
                        debug!(
                            "IR",
                            "Loaded string constant \"{}\" into register {}", s, result_temp
                        );
                    }
                    LiteralValue::Bool(b) => {
                        self.emit(
                            OpCode::LoadConst,
                            vec![
                                Operand::Register(result_temp),
                                Operand::Immediate(value.clone()),
                            ],
                        );
                        debug!(
                            "IR",
                            "Loaded boolean constant {} into register {}", b, result_temp
                        );
                    }
                    _ => {
                        error!("IR", "Unexpected literal type: {:?}", value);
                        panic!("Unexpected literal type");
                    }
                }

                result_temp
            }
            Expr::Variable { name } => {
                if let TokenType::Identifier(name_str) = &name.token_type {
                    debug!("IR", "Generating IR for variable reference '{}'", name_str);

                    let result_temp = self.new_temp();
                    trace!(
                        "IR",
                        "Allocating register {} for variable value",
                        result_temp
                    );

                    self.emit(
                        OpCode::LoadVar,
                        vec![
                            Operand::Register(result_temp),
                            Operand::Variable(name_str.clone()),
                        ],
                    );

                    debug!(
                        "IR",
                        "Loaded variable '{}' into register {}", name_str, result_temp
                    );
                    result_temp
                } else {
                    error!("IR", "Expected identifier, got: {:?}", name.token_type);
                    panic!("Expected identifier for variable name");
                }
            }
        }
    }

    fn emit(&mut self, op: OpCode, operands: Vec<Operand>) {
        trace!(
            "IR",
            "Emitting instruction: {:?} with {} operands at index {}",
            op,
            operands.len(),
            self.instructions.len()
        );

        self.instructions.push(Instruction { op, operands });
    }

    fn new_temp(&mut self) -> usize {
        let temp = self.temp_counter;
        trace!("IR", "Allocating new temporary register {}", temp);
        self.temp_counter += 1;
        temp
    }
}
