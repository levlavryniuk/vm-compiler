use crate::ast_node::*;
use crate::symbol_table::{Symbol, SymbolKind, SymbolTable};
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
    LoadParam, // New: Load parameter from stack
    Label,
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
    Label(String),
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
    symbol_table: SymbolTable,
    label_counter: usize,
    temp_counter: usize,
}

impl IrGenerator {
    pub fn new() -> Self {
        info!("IR", "Creating new IR generator");
        IrGenerator {
            instructions: Vec::new(),
            temp_counter: 0,
            label_counter: 0,
            functions: HashMap::new(),
            symbol_table: SymbolTable::new(),
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
        dbg!(&self.symbol_table);
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
                self.symbol_table.enter_scope();
                let function_start = self.instructions.len();
                trace!(
                    "IR",
                    "Function '{}' starts at instruction index {}",
                    name,
                    function_start
                );

                // Add jump over the function body
                self.emit(OpCode::Jump, vec![Operand::Label(name.clone() + "_end")]);
                trace!("IR", "Added jump placeholder at start of function");

                self.emit(OpCode::Label, vec![Operand::Label(name.clone())]);
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

                for (i, arg) in args.iter().enumerate() {
                    if let Expr::Variable { name: token } = arg {
                        if let TokenType::Identifier(name_str) = &token.token_type {
                            self.symbol_table.insert(
                                name_str,
                                Symbol {
                                    kind: SymbolKind::Param,
                                },
                            );
                            self.emit(
                                OpCode::LoadParam,
                                vec![
                                    Operand::Immediate(LiteralValue::Number(i as f64)),
                                    Operand::Variable(name_str.clone()),
                                ],
                            );
                        }
                    }
                }

                trace!("IR", "Generating IR for function body");
                self.generate_statement(body);
                debug!("IR", "Successfully generated function body");
                self.symbol_table.exit_scope();

                self.emit(OpCode::Return, vec![]);
                trace!("IR", "Added implicit return at end of function");

                let function_end = self.instructions.len() - 1;
                trace!(
                    "IR",
                    "Function '{}' ends at instruction index {}",
                    name,
                    function_end
                );

                self.emit(OpCode::Label, vec![Operand::Label(name.clone() + "_end")]);
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
                        self.symbol_table.insert(
                            name_str,
                            Symbol {
                                kind: SymbolKind::Variable,
                            },
                        );
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
                let jump_if_false_label =
                    format!("if_false_{}_{}", jump_if_false_idx, self.label_counter);
                self.label_counter += 1;
                trace!("IR", "Adding JumpIfFalse at index {}", jump_if_false_idx);

                self.emit(
                    OpCode::JumpIfFalse,
                    vec![
                        Operand::Register(condition_temp),
                        Operand::Label(jump_if_false_label.clone()),
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

                    let jump_end_label = format!("if_end_{}_{}", jump_end_idx, self.label_counter);
                    self.label_counter += 1;
                    self.emit(OpCode::Jump, vec![Operand::Label(jump_end_label.clone())]);

                    let else_start_idx = self.instructions.len();
                    trace!("IR", "Else branch starts at index {}", else_start_idx);

                    trace!("IR", "Generating IR for else branch");
                    self.emit(
                        OpCode::Label,
                        vec![Operand::Label(jump_if_false_label.clone())],
                    );
                    self.generate_statement(else_branch);
                    debug!("IR", "Successfully generated IR for else branch");

                    let end_idx = self.instructions.len();
                    trace!("IR", "If-else statement ends at index {}", end_idx);
                    self.emit(OpCode::Label, vec![Operand::Label(jump_end_label.clone())]);

                    debug!("IR", "Successfully generated complete if-else statement");
                } else {
                    debug!("IR", "If statement has no else branch");
                    let end_idx = self.instructions.len();
                    trace!("IR", "If statement ends at index {}", end_idx);

                    self.emit(
                        OpCode::Label,
                        vec![Operand::Label(jump_if_false_label.clone())],
                    );

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
                    if let Some(symbol) = self.symbol_table.lookup(name_str) {
                        match symbol.kind {
                            SymbolKind::Param | SymbolKind::Variable => {
                                let temp = self.new_temp();
                                self.emit(
                                    OpCode::LoadVar,
                                    vec![
                                        Operand::Variable(name_str.clone()),
                                        Operand::Register(temp),
                                    ],
                                );
                                temp
                            }
                            _ => {
                                // Handle other symbol kinds if needed
                                error!("IR", "Unexpected symbol kind");
                                0
                            }
                        }
                    } else {
                        error!("IR", "Variable not found");
                        0
                    }
                } else {
                    error!("IR", "Unexpected token type in variable expression");
                    0
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
    pub fn log_ir_without_labels(&self) {
        println!("--- Generated IR Instructions without labels ---");
        for (index, instruction) in self.instructions.iter().enumerate() {
            print!("{:04}: {:?} ", index, instruction.op);
            for (operand_index, operand) in instruction.operands.iter().enumerate() {
                print!("{:?}", operand);
                if operand_index < instruction.operands.len() - 1 {
                    print!(", ");
                }
            }
            println!();
        }
    }
    pub fn log_instructions(&self) {
        println!("--- Generated IR Instructions ---");
        for (index, instruction) in self.instructions.iter().enumerate() {
            print!("{:04}: {:?} ", index, instruction.op);
            for (operand_index, operand) in instruction.operands.iter().enumerate() {
                print!("{:?}", operand);
                if operand_index < instruction.operands.len() - 1 {
                    print!(", ");
                }
            }
            println!();
        }
        println!("--- End of IR Instructions ---");
    }
}
