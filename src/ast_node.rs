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
}

#[derive(Debug)]
pub enum LiteralValue {
    Number(f64),
    Boolean(bool),
    Nil,
}

#[derive(Debug)]
pub enum Stmt {
    Expression {
        expression: Expr,
    },
    Let {
        name: Token,
        initializer: Option<Expr>,
    },
}
