use crate::{
    ast_node::{Expr, LiteralValue, Stmt},
    tokens::TokenType,
};

#[derive(Debug, Clone)]
pub enum OpCode {
    LoadConst,
    LoadVar,
    StoreVar,

    Add,
    Subtract,
    Multiply,
    Divide,

    Equals,
    NotEquals,
    Greater,
    Less,
    GreaterOrEq,
    LessOrEq,

    Negate,

    Echo,

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
}

pub struct IrGenerator {
    instructions: Vec<Instruction>,
    temp_counter: usize,
}

impl IrGenerator {
    pub fn new() -> Self {
        IrGenerator {
            instructions: Vec::new(),
            temp_counter: 0,
        }
    }

    pub fn generate(&mut self, statements: &[Stmt]) -> IrProgram {
        for stmt in statements {
            self.generate_statement(stmt);
        }

        IrProgram {
            instructions: self.instructions.clone(),
        }
    }

    fn generate_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression { expression } => {
                self.generate_expression(expression);
            }
            Stmt::FunctionDeclaration { args, name, body } => {}
            Stmt::Echo { expression } => {
                let expr_temp = self.generate_expression(expression);

                self.emit(OpCode::Echo, vec![Operand::Register(expr_temp)]);
            }
            Stmt::Let { name, initializer } => {
                if let Some(init) = initializer {
                    let temp = self.generate_expression(init);

                    if let TokenType::Identifier(name_str) = &name.token_type {
                        self.emit(
                            OpCode::StoreVar,
                            vec![Operand::Variable(name_str.clone()), Operand::Register(temp)],
                        );
                    }
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_temp = self.generate_expression(condition);
                let jump_if_false_idx = self.instructions.len();
                self.emit(
                    OpCode::JumpIfFalse,
                    vec![
                        Operand::Register(condition_temp),
                        Operand::Immediate(LiteralValue::Number(0.)),
                    ],
                );

                self.generate_statement(then_branch);
                if let Some(else_branch) = else_branch {
                    let jump_end_idx = self.instructions.len();
                    self.emit(
                        OpCode::Jump,
                        vec![Operand::Immediate(LiteralValue::Number(0.0))],
                    );

                    let else_start_idx = self.instructions.len();

                    if let Operand::Immediate(LiteralValue::Number(ref mut value)) =
                        self.instructions[jump_if_false_idx].operands[1]
                    {
                        *value = else_start_idx as f64;
                    }

                    self.generate_statement(else_branch);

                    let end_idx = self.instructions.len();
                    if let Operand::Immediate(LiteralValue::Number(ref mut value)) =
                        self.instructions[jump_end_idx].operands[0]
                    {
                        *value = end_idx as f64;
                    }
                } else {
                    let end_idx = self.instructions.len();
                    if let Operand::Immediate(LiteralValue::Number(ref mut value)) =
                        self.instructions[jump_if_false_idx].operands[1]
                    {
                        *value = end_idx as f64;
                    }
                }
            }
            Stmt::Block { statements } => {
                for st in statements {
                    self.generate_statement(st);
                }
            }
        }
    }

    fn generate_expression(&mut self, expr: &Expr) -> usize {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left_temp = self.generate_expression(left);
                let right_temp = self.generate_expression(right);
                let result_temp = self.new_temp();

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
                    _ => panic!("Unexpected binary operator"),
                };

                self.emit(
                    op,
                    vec![
                        Operand::Register(result_temp),
                        Operand::Register(left_temp),
                        Operand::Register(right_temp),
                    ],
                );

                result_temp
            }
            Expr::Unary { operator, right } => {
                let right_temp = self.generate_expression(right);
                let result_temp = self.new_temp();

                match operator.token_type {
                    TokenType::Minus => {
                        self.emit(
                            OpCode::Negate,
                            vec![
                                Operand::Register(result_temp),
                                Operand::Register(right_temp),
                            ],
                        );
                    }
                    _ => panic!("Unexpected unary operator"),
                }

                result_temp
            }
            Expr::Grouping { expression } => self.generate_expression(expression),
            Expr::Literal { value } => {
                let result_temp = self.new_temp();

                match value {
                    LiteralValue::Number(_) => {
                        self.emit(
                            OpCode::LoadConst,
                            vec![
                                Operand::Register(result_temp),
                                Operand::Immediate(value.clone()),
                            ],
                        );
                    }
                    LiteralValue::String(_) => {
                        self.emit(
                            OpCode::LoadConst,
                            vec![
                                Operand::Register(result_temp),
                                Operand::Immediate(value.clone()),
                            ],
                        );
                    }
                    LiteralValue::Bool(_) => {
                        self.emit(
                            OpCode::LoadConst,
                            vec![
                                Operand::Register(result_temp),
                                Operand::Immediate(value.clone()),
                            ],
                        );
                    }
                    _ => panic!("Unexpected literal type"),
                }

                result_temp
            }
            Expr::Variable { name } => {
                let result_temp = self.new_temp();

                if let TokenType::Identifier(name_str) = &name.token_type {
                    self.emit(
                        OpCode::LoadVar,
                        vec![
                            Operand::Register(result_temp),
                            Operand::Variable(name_str.clone()),
                        ],
                    );
                }

                result_temp
            }
        }
    }

    fn emit(&mut self, op: OpCode, operands: Vec<Operand>) {
        self.instructions.push(Instruction { op, operands });
    }

    fn new_temp(&mut self) -> usize {
        let temp = self.temp_counter;
        self.temp_counter += 1;
        temp
    }
}
