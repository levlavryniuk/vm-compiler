use std::collections::HashMap;

use crate::{
    ast_node::{Expr, LiteralValue, Stmt},
    tokens::TokenType,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    Boolean,
    Nil,
    Unknown,
}

pub struct Environment {
    values: HashMap<String, Type>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn with_enclosing(enclosing: Environment) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(Box::new(enclosing)),
        }
    }

    /// defines a variable with name and type;
    pub fn define(&mut self, name: &str, value_type: Type) {
        self.values.insert(name.to_string(), value_type);
    }

    // gets a type of variable by its name recursively to higher env;
    pub fn get(&self, name: &str) -> Option<Type> {
        if let Some(value_type) = self.values.get(name) {
            return Some(value_type.clone());
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.get(name);
        }

        None
    }
}

pub struct TypeChecker {
    environment: Environment,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            environment: Environment::new(),
        }
    }

    pub fn check(&mut self, statements: &[Stmt]) -> Result<(), String> {
        for statement in statements {
            self.check_statement(statement)?;
        }

        Ok(())
    }

    fn check_statement(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Expression { expression } => {
                self.check_expression(expression)?;
                Ok(())
            }
            Stmt::Let { name, initializer } => {
                let var_type = if let Some(init) = initializer {
                    self.check_expression(init)?
                } else {
                    Type::Nil
                };

                if let TokenType::Identifier(name_str) = &name.token_type {
                    self.environment.define(name_str, var_type);
                    Ok(())
                } else {
                    Err("Expected identifier".to_string())
                }
            }
        }
    }

    /// this returns type of expression;
    fn check_expression(&mut self, expr: &Expr) -> Result<Type, String> {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                match operator.token_type {
                    TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Slash => {
                        if left_type != Type::Number || right_type != Type::Number {
                            return Err(format!(
                                "Operands must be numbers, got {:?} and {:?}",
                                left_type, right_type
                            ));
                        }

                        Ok(Type::Number)
                    }
                    _ => Err(format!("Unknown binary operator: {:?}", operator)),
                }
            }
            Expr::Unary { operator, right } => {
                let right_type = self.check_expression(right)?;

                match operator.token_type {
                    TokenType::Minus => {
                        if right_type != Type::Number {
                            return Err(format!("Operand must be a number, got {:?}", right_type));
                        }

                        Ok(Type::Number)
                    }
                    _ => Err(format!("Unknown unary operator: {:?}", operator)),
                }
            }
            Expr::Grouping { expression } => self.check_expression(expression),
            Expr::Literal { value } => match value {
                LiteralValue::Number(_) => Ok(Type::Number),
                LiteralValue::Boolean(_) => Ok(Type::Boolean),
                LiteralValue::Nil => Ok(Type::Nil),
            },
            Expr::Variable { name } => {
                if let TokenType::Identifier(name_str) = &name.token_type {
                    if let Some(var_type) = self.environment.get(name_str) {
                        Ok(var_type)
                    } else {
                        Err(format!("Undefined variable '{}'", name_str))
                    }
                } else {
                    Err("Expected identifier".to_string())
                }
            }
        }
    }
}
