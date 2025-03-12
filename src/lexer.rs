use crate::tokens::{Token, TokenType};

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Lexer {
            input: source.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(token) = self.next_token() {
            let token_type = token.token_type.clone();
            tokens.push(token);
            if TokenType::EOF.eq(&token_type) {
                // if met end of file - finish tokenizing
                break;
            }
        }

        tokens
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.is_at_end() {
            return Some(self.make_token(TokenType::EOF));
        }

        // skipped through all the whitespace - start from the first non-whitespace char
        let c = self.advance();

        match c {
            '(' => Some(self.make_token(TokenType::LeftParen)),
            ')' => Some(self.make_token(TokenType::RightParen)),
            '+' => Some(self.make_token(TokenType::Plus)),
            '-' => Some(self.make_token(TokenType::Minus)),
            '*' => Some(self.make_token(TokenType::Star)),
            '/' => Some(self.make_token(TokenType::Slash)),
            '=' => Some(self.make_token(TokenType::Equal)),

            '0'..='9' => {
                self.retreat(); // Go back to reprocess the digit
                Some(self.number())
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                self.retreat(); // Go back to reprocess the letter
                Some(self.identifier())
            }

            _ => {
                eprintln!("Unexpected character: {}", c);
                self.next_token()
            }
        }
    }

    fn number(&mut self) -> Token {
        let start_pos = self.position;

        // loop until number is ended. start_pos -> self.position is the "number"
        while !self.is_at_end() && self.peek().is_digit(10) {
            self.advance();
        }

        // if prev loop stopped because of non-digit - it is probably a decimal ".",  so skip it
        // and go back to number proccessing
        if !self.is_at_end() && self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance(); // Consume the '.'

            while !self.is_at_end() && self.peek().is_digit(10) {
                self.advance();
            }
        }

        let number_str: String = self.input[start_pos..self.position].iter().collect();
        let value = number_str.parse::<f64>().unwrap();

        self.make_token(TokenType::Number(value))
    }

    fn identifier(&mut self) -> Token {
        let start_pos = self.position;

        // if this is a variable, that has chars and numbers and _
        while !self.is_at_end() && (self.peek().is_alphanumeric() || self.peek() == '_') {
            self.advance();
        }

        let identifier: String = self.input[start_pos..self.position].iter().collect();

        // Check for keywords
        let token_type = match identifier.as_str() {
            "let" => TokenType::Let,
            _ => TokenType::Identifier(identifier),
        };

        self.make_token(token_type)
    }

    // Helper methods
    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }

    fn advance(&mut self) -> char {
        let c = self.input[self.position];
        self.position += 1;
        self.column += 1;

        if c == '\n' {
            self.line += 1;
            self.column = 1;
        }

        c
    }

    fn retreat(&mut self) {
        self.position -= 1;
        self.column -= 1;
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.input[self.position]
        }
    }

    fn peek_next(&self) -> char {
        if self.position + 1 >= self.input.len() {
            '\0'
        } else {
            self.input[self.position + 1]
        }
    }

    /// Skips all whitespace (' '), tabs ('\t'), empty lines ('\n') and comments
    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                    self.advance();
                }
                // Skip comments
                '/' if self.peek_next() == '/' => {
                    while !self.is_at_end() && self.peek() != '\n' {
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        Token {
            token_type,
            line: self.line,
            column: self.column,
        }
    }
}
