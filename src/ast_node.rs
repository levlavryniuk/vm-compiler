use std::fmt::Display;

use crate::tokens::Token;

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: LiteralValue,
    },
    Variable {
        name: Token,
    },
    Call {
        callee: String,
        arguments: Vec<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Number(f64),
    Bool(bool),
    String(String),
    Nil,
}
impl PartialOrd for LiteralValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (LiteralValue::Number(num1), LiteralValue::Number(num2)) => num1.partial_cmp(num2),
            (LiteralValue::String(str1), LiteralValue::String(str2)) => str1.partial_cmp(str2),
            (LiteralValue::Bool(bool1), LiteralValue::Bool(bool2)) => bool1.partial_cmp(bool2),
            (LiteralValue::Nil, LiteralValue::Nil) => Some(std::cmp::Ordering::Equal),

            _ => None,
        }
    }
}

impl PartialEq for LiteralValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LiteralValue::Number(num1), LiteralValue::Number(num2)) => num1 == num2,
            (LiteralValue::String(str1), LiteralValue::String(str2)) => str1 == str2,
            (LiteralValue::Bool(bool1), LiteralValue::Bool(bool2)) => bool1 == bool2,
            (LiteralValue::Nil, LiteralValue::Nil) => true,
            _ => false,
        }
    }
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Nil => write!(f, "Nil"),
            Self::String(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Expression {
        expression: Expr,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Echo {
        expression: Expr,
    },
    Block {
        statements: Vec<Stmt>,
    },

    Return {
        value: Option<Expr>,
    },
    FunctionDeclaration {
        args: Vec<Expr>,
        name: String,
        body: Box<Stmt>,
        returns_value: bool,
    },
    Let {
        name: Token,
        initializer: Option<Expr>,
    },
}
