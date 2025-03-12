#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Literals
    Number(f64),
    Identifier(String),

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Equal,

    // Delimiters
    LeftParen,
    RightParen,

    // Keywords
    Let,

    // Control
    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
}
