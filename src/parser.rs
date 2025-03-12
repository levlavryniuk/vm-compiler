use crate::ast_node::*;
use crate::tokens::*;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            match self.statement() {
                Ok(stmt) => statements.push(stmt),
                Err(error) => return Err(error),
            }
        }

        Ok(statements)
    }

    fn statement(&mut self) -> Result<Stmt, String> {
        if self.match_token(TokenType::Let) {
            self.let_statement()
        } else {
            self.expression_statement()
        }
    }

    fn let_statement(&mut self) -> Result<Stmt, String> {
        let name = self.consume(
            |t| matches!(t, TokenType::Identifier(_)),
            "Expect variable name.",
        )?;

        let mut initializer = None;

        if self.match_token(TokenType::Equal) {
            initializer = Some(self.expression()?);
        }

        Ok(Stmt::Let { name, initializer })
    }

    fn expression_statement(&mut self) -> Result<Stmt, String> {
        let expr = self.expression()?;
        Ok(Stmt::Expression { expression: expr })
    }

    // Expression grammar rules - following the grammar from earlier
    fn expression(&mut self) -> Result<Expr, String> {
        self.term()
    }

    fn term(&mut self) -> Result<Expr, String> {
        let mut expr = self.factor()?;

        while self.match_tokens(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;

        while self.match_tokens(&[TokenType::Star, TokenType::Slash]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        if self.match_tokens(&[TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;

            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, String> {
        if self.match_token(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume(
                |t| *t == TokenType::RightParen,
                "Expect ')' after expression.",
            )?;

            return Ok(Expr::Grouping {
                expression: Box::new(expr),
            });
        }

        if let Ok(token) = self.consume(|t| matches!(t, TokenType::Number(_)), "Expect expression.")
        {
            if let TokenType::Number(value) = token.token_type {
                return Ok(Expr::Literal {
                    value: LiteralValue::Number(value),
                });
            }
        }

        if let Ok(token) = self.consume(
            |t| matches!(t, TokenType::Identifier(_)),
            "Expect expression.",
        ) {
            return Ok(Expr::Variable { name: token });
        }

        Err(format!("Unexpected token: {:?}", self.peek()))
    }

    // Helper methods
    fn match_token(&mut self, token_type: TokenType) -> bool {
        if self.check(|t| *t == token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_tokens(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(|t| t == token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    /// Basically skips token if predicate is true
    /// If predicate returns true -> we skip to next token and return the old one.
    /// if returns false -> errors
    fn consume<F>(&mut self, predicate: F, error_message: &str) -> Result<Token, String>
    where
        F: Fn(&TokenType) -> bool,
    {
        if self.check(&predicate) {
            Ok(self.advance())
        } else {
            Err(format!("{} Got {:?}", error_message, self.peek()))
        }
    }

    fn check<F>(&self, predicate: F) -> bool
    where
        F: Fn(&TokenType) -> bool,
    {
        if self.is_at_end() {
            false
        } else {
            predicate(&self.peek().token_type)
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous().clone()
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().token_type, TokenType::EOF)
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}
