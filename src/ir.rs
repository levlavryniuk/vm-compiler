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
    Negate,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub op: OpCode,
    pub operands: Vec<Operand>,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Register(usize),
    Immediate(f64),
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
                    LiteralValue::Number(num) => {
                        self.emit(
                            OpCode::LoadConst,
                            vec![Operand::Register(result_temp), Operand::Immediate(*num)],
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
