use std::{collections::HashMap, ops::Deref};

use crate::{
    ast_node::{Expr, LiteralValue, Stmt},
    tokens::TokenType,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    Bool,
    Nil,
    String,
    Unknown,
}

#[derive(Clone)]
pub struct Context {
    values: HashMap<String, Type>,
    parent_context: Option<Box<Context>>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            values: HashMap::new(),
            parent_context: None,
        }
    }

    pub fn with_parent(ctx: Context) -> Self {
        Context {
            values: HashMap::new(),
            parent_context: Some(Box::new(ctx)),
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

        if let Some(enclosing) = &self.parent_context {
            return enclosing.get(name);
        }

        None
    }
}

pub struct TypeChecker {}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {}
    }

    pub fn check(&mut self, ctx: &mut Context, statements: &[Stmt]) -> Result<(), String> {
        for statement in statements {
            self.check_statement(ctx, statement)?;
        }

        Ok(())
    }

    fn check_statement(&mut self, context: &mut Context, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Expression { expression } => {
                self.check_expression(context, expression)?;
                Ok(())
            }
            Stmt::Let { name, initializer } => {
                let var_type = if let Some(init) = initializer {
                    self.check_expression(context, init)?
                } else {
                    Type::Nil
                };

                if let TokenType::Identifier(name_str) = &name.token_type {
                    context.define(name_str, var_type);
                    Ok(())
                } else {
                    Err("Expected identifier".to_string())
                }
            }
            Stmt::FunctionDeclaration { args, name, body } => Ok(()),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_type = self.check_expression(context, condition)?;

                if condition_type != Type::Bool {
                    return Err("Condition must be of type `bool`".into());
                }

                let mut then_branch_ctx = Context::with_parent(context.clone());
                if let Stmt::Block { statements } = then_branch.deref() {
                    self.check(&mut then_branch_ctx, &statements)?;
                }
                let mut else_branch_ctx = Context::with_parent(context.clone());
                if let Some(stmt) = else_branch {
                    if let Stmt::Block { statements } = stmt.deref() {
                        self.check(&mut else_branch_ctx, &statements)?;
                    }
                }

                Ok(())
            }
            Stmt::Echo { .. } => Ok(()),
            Stmt::Block { statements } => Ok(statements
                .iter()
                .try_for_each(|s| self.check_statement(context, s))?),
        }
    }

    /// this returns type of expression;
    fn check_expression(&mut self, ctx: &mut Context, expr: &Expr) -> Result<Type, String> {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left_type = self.check_expression(ctx, left)?;
                let right_type = self.check_expression(ctx, right)?;

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
                    TokenType::BangEqual | TokenType::EqualEqual => {
                        if left_type != right_type {
                            return Err(format!(
                                "Operands must be of the same type, got {:?} and {:?}",
                                left_type, right_type
                            ));
                        } else {
                            Ok(Type::Bool)
                        }
                    }
                    TokenType::Greater
                    | TokenType::Less
                    | TokenType::GreaterOrEq
                    | TokenType::LessOrEq => {
                        if left_type != Type::Number || right_type != Type::Number {
                            return Err(format!(
                                "Comparison operators require number operands, got {:?} and {:?}",
                                left_type, right_type
                            ));
                        }

                        // All comparison operators yield a boolean result
                        Ok(Type::Bool)
                    }
                    _ => Err(format!("Unknown binary operator: {:?}", operator)),
                }
            }
            Expr::Unary { operator, right } => {
                let right_type = self.check_expression(ctx, right)?;

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
            Expr::Grouping { expression } => self.check_expression(ctx, expression),
            Expr::Literal { value } => match value {
                LiteralValue::Number(_) => Ok(Type::Number),
                LiteralValue::Bool(_) => Ok(Type::Bool),
                LiteralValue::Nil => Ok(Type::Nil),
                LiteralValue::String(_) => Ok(Type::String),
            },
            Expr::Variable { name } => {
                if let TokenType::Identifier(name_str) = &name.token_type {
                    if let Some(var_type) = ctx.get(name_str) {
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
