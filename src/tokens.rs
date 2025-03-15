use crate::ast_node::Expr;
pub struct FunctionCallData {
    args: Vec<Expr>,
}
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Literals
    Number(f64),
    Identifier(String),
    Bool(bool),
    String(String),

    // Operators
    Plus,
    Minus,
    Star,
    Slash,

    //comaprison
    Equal,
    BangEqual,
    EqualEqual,
    Greater,
    Less,
    GreaterOrEq,
    LessOrEq,

    //logical ops
    LogicalNot,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    DoubleQuotes,

    FunctionDeclaration,
    FunctionCall,
    Comma,

    Let,
    If,
    Else,
    Echo,
    // Control
    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
}
