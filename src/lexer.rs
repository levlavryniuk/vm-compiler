use crate::tokens::{Token, TokenType};
use crate::{debug, error, info, trace};

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        info!(
            "Lexer",
            "Creating new lexer instance with {} chars",
            source.len()
        );
        Lexer {
            input: source.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        info!("Lexer", "Starting tokenization");
        let mut tokens = Vec::new();

        while let Some(token) = self.next_token() {
            let token_type = token.token_type.clone();
            debug!(
                "Lexer",
                "Found token: {:?} at line {} column {}",
                token.token_type,
                token.line,
                token.column
            );
            tokens.push(token);
            if TokenType::EOF.eq(&token_type) {
                // if met end of file - finish tokenizing
                info!("Lexer", "Reached EOF, tokenization complete");
                break;
            }
        }

        info!(
            "Lexer",
            "Tokenization complete, found {} tokens",
            tokens.len()
        );
        tokens
    }

    fn next_token(&mut self) -> Option<Token> {
        trace!(
            "Lexer",
            "Looking for next token at position {}",
            self.position
        );
        self.skip_whitespace();

        if self.is_at_end() {
            debug!("Lexer", "Reached end of input");
            return Some(self.make_token(TokenType::EOF));
        }

        // skipped through all the whitespace - start from the first non-whitespace char
        let c = self.advance();
        trace!("Lexer", "Processing character: '{}'", c);

        match c {
            '(' => Some(self.make_token(TokenType::LeftParen)),
            ')' => Some(self.make_token(TokenType::RightParen)),
            '{' => Some(self.make_token(TokenType::LeftBrace)),
            '}' => Some(self.make_token(TokenType::RightBrace)),
            '+' => Some(self.make_token(TokenType::Plus)),
            '-' => Some(self.make_token(TokenType::Minus)),
            '*' => Some(self.make_token(TokenType::Star)),
            '/' => Some(self.make_token(TokenType::Slash)),
            ',' => Some(self.make_token(TokenType::Comma)),
            '!' => {
                let next = self.peek();
                trace!(
                    "Lexer",
                    "Checking if '!' is followed by '=': next = '{}'",
                    next
                );
                if self.match_char('=') {
                    Some(self.make_token(TokenType::BangEqual))
                } else {
                    Some(self.make_token(TokenType::LogicalNot))
                }
            }
            '<' => {
                let next = self.peek();
                trace!(
                    "Lexer",
                    "Checking if '<' is followed by '=': next = '{}'",
                    next
                );
                if self.match_char('=') {
                    Some(self.make_token(TokenType::LessOrEq))
                } else {
                    Some(self.make_token(TokenType::Less))
                }
            }
            '>' => {
                let next = self.peek();
                trace!(
                    "Lexer",
                    "Checking if '>' is followed by '=': next = '{}'",
                    next
                );
                if self.match_char('=') {
                    Some(self.make_token(TokenType::GreaterOrEq))
                } else {
                    Some(self.make_token(TokenType::Greater))
                }
            }

            '=' => {
                let next = self.peek();
                trace!(
                    "Lexer",
                    "Checking if '=' is followed by '=': next = '{}'",
                    next
                );
                if self.match_char('=') {
                    Some(self.make_token(TokenType::EqualEqual))
                } else {
                    Some(self.make_token(TokenType::Equal))
                }
            }

            '"' => Some(self.string()),

            '0'..='9' => {
                self.retreat(); // Go back to reprocess the digit
                Some(self.number())
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                self.retreat(); // Go back to reprocess the letter
                Some(self.identifier())
            }

            _ => {
                error!(
                    "Lexer",
                    "Unexpected character: '{}' at line {} column {}", c, self.line, self.column
                );
                self.next_token()
            }
        }
    }

    fn string(&mut self) -> Token {
        debug!(
            "Lexer",
            "Processing string literal at line {} column {}", self.line, self.column
        );
        let mut contents = String::new();
        let start_line = self.line;
        let start_column = self.column;

        while self.peek() != '\"' && !self.is_at_end() {
            contents.push(self.peek());
            self.advance();
        }

        if self.is_at_end() {
            error!(
                "Lexer",
                "Unterminated string starting at line {} column {}", start_line, start_column
            );
            // We continue despite the error to avoid crashing the lexer
        } else {
            // Consume the closing "
            self.advance();
        }

        debug!("Lexer", "String literal: \"{}\"", contents);

        Token {
            column: start_column,
            line: start_line,
            token_type: TokenType::String(contents),
        }
    }

    fn number(&mut self) -> Token {
        debug!(
            "Lexer",
            "Processing number at line {} column {}", self.line, self.column
        );
        let start_pos = self.position;
        let start_line = self.line;
        let start_column = self.column;

        // loop until number is ended. start_pos -> self.position is the "number"
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            self.advance();
        }

        // if prev loop stopped because of non-digit - it is probably a decimal ".",  so skip it
        // and go back to number proccessing
        if !self.is_at_end() && self.peek() == '.' && self.peek_next().is_ascii_digit() {
            trace!("Lexer", "Found decimal point in number");
            self.advance(); // Consume the '.'

            while !self.is_at_end() && self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let number_str: String = self.input[start_pos..self.position].iter().collect();
        let value = match number_str.parse::<f64>() {
            Ok(v) => {
                debug!("Lexer", "Parsed number: {}", v);
                v
            }
            Err(e) => {
                error!("Lexer", "Failed to parse number '{}': {}", number_str, e);
                0.0 // Default value to avoid crashing
            }
        };

        Token {
            line: start_line,
            column: start_column,
            token_type: TokenType::Number(value),
        }
    }

    fn identifier(&mut self) -> Token {
        debug!(
            "Lexer",
            "Processing identifier at line {} column {}", self.line, self.column
        );
        let start_pos = self.position;
        let start_line = self.line;
        let start_column = self.column;

        // if this is a variable, that has chars and numbers and _
        while !self.is_at_end() && (self.peek().is_alphanumeric() || self.peek() == '_') {
            self.advance();
        }

        let identifier: String = self.input[start_pos..self.position].iter().collect();
        trace!("Lexer", "Identifier or keyword: '{}'", identifier);

        // Check for keywords
        let token_type = match identifier.as_str() {
            "let" => TokenType::Let,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "true" => TokenType::Bool(true),
            "false" => TokenType::Bool(false),
            "echo" => TokenType::Echo,
            "fn" => TokenType::FunctionDeclaration,
            "return" => TokenType::Return,
            _ => TokenType::Identifier(identifier),
        };

        debug!("Lexer", "Identified as: {:?}", token_type);

        Token {
            line: start_line,
            column: start_column,
            token_type,
        }
    }

    // Helper methods
    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }

    fn advance(&mut self) -> char {
        if self.is_at_end() {
            trace!("Lexer", "Attempted to advance past end of input");
            return '\0';
        }

        let c = self.input[self.position];
        self.position += 1;
        self.column += 1;

        if c == '\n' {
            self.line += 1;
            self.column = 1;
            trace!("Lexer", "Advanced to new line {}", self.line);
        }

        c
    }

    fn retreat(&mut self) {
        if self.position > 0 {
            self.position -= 1;
            self.column -= 1;
            trace!(
                "Lexer",
                "Retreated to position {}, column {}",
                self.position,
                self.column
            );
        } else {
            error!("Lexer", "Attempted to retreat before start of input");
        }
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

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.advance();
            true
        }
    }

    /// Skips all whitespace (' '), tabs ('\t'), empty lines ('\n') and comments
    fn skip_whitespace(&mut self) {
        trace!(
            "Lexer",
            "Skipping whitespace starting at position {}",
            self.position
        );
        let start_pos = self.position;

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
                    trace!("Lexer", "Skipping comment at line {}", self.line);
                    while !self.is_at_end() && self.peek() != '\n' {
                        self.advance();
                    }
                }
                _ => break,
            }
        }

        if self.position > start_pos {
            trace!(
                "Lexer",
                "Skipped {} whitespace characters",
                self.position - start_pos
            );
        }
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        trace!(
            "Lexer",
            "Creating token: {:?} at line {} column {}",
            token_type,
            self.line,
            self.column
        );
        Token {
            token_type,
            line: self.line,
            column: self.column,
        }
    }
}
