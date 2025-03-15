use crate::ast_node::*;
use crate::tokens::*;
use crate::{debug, error, info, trace, warn};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        info!("Parser", "Creating new parser with {} tokens", tokens.len());
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        info!("Parser", "Starting parsing");
        let mut statements = Vec::new();

        while !self.is_at_end() {
            trace!(
                "Parser",
                "Parsing statement at token index {}",
                self.current
            );
            match self.statement() {
                Ok(stmt) => {
                    debug!("Parser", "Successfully parsed statement: {:?}", stmt);
                    statements.push(stmt);
                }
                Err(error) => {
                    error!("Parser", "Failed to parse statement: {}", error);
                    return Err(error);
                }
            }
        }

        info!(
            "Parser",
            "Parsing complete, found {} statements",
            statements.len()
        );
        Ok(statements)
    }

    fn statement(&mut self) -> Result<Stmt, String> {
        let token = self.peek();
        trace!(
            "Parser",
            "Determining statement type from token: {:?}",
            token.token_type
        );

        match &token.token_type {
            TokenType::Let => {
                debug!("Parser", "Parsing let statement at line {}", token.line);
                self.advance();
                self.let_statement()
            }
            TokenType::If => {
                debug!("Parser", "Parsing if statement at line {}", token.line);
                self.advance();
                self.if_statement()
            }
            TokenType::Echo => {
                debug!("Parser", "Parsing echo statement at line {}", token.line);
                self.advance();
                self.echo_statement()
            }
            TokenType::FunctionDeclaration => {
                debug!(
                    "Parser",
                    "Parsing function declaration at line {}", token.line
                );
                self.advance();
                self.function_declaration()
            }
            _ => {
                debug!(
                    "Parser",
                    "Parsing expression statement at line {}", token.line
                );
                self.expression_statement()
            }
        }
    }

    fn function_declaration(&mut self) -> Result<Stmt, String> {
        trace!("Parser", "Looking for function name");

        let function_name = match self.consume_advance_if(
            |t| matches!(t, TokenType::Identifier(_)),
            "Expected function name",
        ) {
            Ok(Token {
                token_type: TokenType::Identifier(name),
                ..
            }) => {
                debug!("Parser", "Found function name: {}", name);
                name
            }
            Ok(_) => {
                error!(
                    "Parser",
                    "Expected identifier token for function name, but got something else"
                );
                return Err(String::from("Expected function name"));
            }
            Err(e) => {
                error!("Parser", "Failed to get function name: {}", e);
                return Err(e);
            }
        };

        trace!("Parser", "Looking for left parenthesis");
        if self.match_token(TokenType::LeftParen) {
            debug!("Parser", "Parsing function arguments");
            let mut args = Vec::new();

            while !self.match_token(TokenType::RightParen) {
                trace!("Parser", "Parsing function argument");
                let arg = match self.expression() {
                    Ok(expr) => {
                        debug!("Parser", "Added function argument: {:?}", expr);
                        expr
                    }
                    Err(e) => {
                        error!("Parser", "Failed to parse function argument: {}", e);
                        return Err(e);
                    }
                };
                args.push(arg);

                if !self.match_token(TokenType::Comma) {
                    trace!("Parser", "No more function arguments");
                    self.advance();
                    break;
                }
                trace!("Parser", "Found comma, expecting another argument");
            }
            debug!("Parser", "Function has {} arguments", args.len());

            trace!("Parser", "Looking for function body");
            if self.match_token(TokenType::LeftBrace) {
                debug!("Parser", "Parsing function body");
                let function_body = match self.block() {
                    Ok(body) => body,
                    Err(e) => {
                        error!("Parser", "Failed to parse function body: {}", e);
                        return Err(e);
                    }
                };

                debug!(
                    "Parser",
                    "Successfully parsed function declaration '{}'", function_name
                );
                return Ok(Stmt::FunctionDeclaration {
                    args,
                    name: function_name,
                    body: Box::new(function_body),
                });
            } else {
                error!("Parser", "Expected {{, found {:?}", self.peek());
                Err(String::from("Expected function body"))
            }
        } else {
            error!("Parser", "Expected (, found {:?}", self.peek());
            Err(String::from("Expected left paren"))
        }
    }

    fn let_statement(&mut self) -> Result<Stmt, String> {
        trace!("Parser", "Looking for variable name in let statement");
        let name = match self.consume_advance_if(
            |t| matches!(t, TokenType::Identifier(_)),
            "Expect variable name.",
        ) {
            Ok(token) => {
                if let TokenType::Identifier(ref name) = token.token_type {
                    debug!("Parser", "Found variable name: {}", name);
                    token
                } else {
                    error!(
                        "Parser",
                        "Unexpected token type after passing identifier check"
                    );
                    return Err(String::from("Expected variable name"));
                }
            }
            Err(e) => {
                error!("Parser", "Failed to get variable name: {}", e);
                return Err(e);
            }
        };

        let mut initializer = None;

        trace!("Parser", "Checking for variable initializer");
        if self.match_token(TokenType::Equal) {
            debug!("Parser", "Parsing variable initializer");
            initializer = match self.expression() {
                Ok(expr) => {
                    debug!("Parser", "Successfully parsed variable initializer");
                    Some(expr)
                }
                Err(e) => {
                    error!("Parser", "Failed to parse initializer: {}", e);
                    return Err(e);
                }
            };
        } else {
            debug!("Parser", "No initializer for variable");
        }

        let var_name = if let TokenType::Identifier(ref name) = name.token_type {
            name
        } else {
            "unknown"
        };
        debug!(
            "Parser",
            "Successfully parsed let statement for '{}'", var_name
        );
        Ok(Stmt::Let { name, initializer })
    }

    fn block(&mut self) -> Result<Stmt, String> {
        debug!("Parser", "Parsing block statement");
        let mut statements = Vec::new();

        while !self.is_at_end() && !self.check(|t| *t == TokenType::RightBrace) {
            trace!("Parser", "Parsing statement inside block");
            match self.statement() {
                Ok(stmt) => {
                    debug!("Parser", "Added statement to block");
                    statements.push(stmt);
                }
                Err(e) => {
                    error!("Parser", "Failed to parse statement in block: {}", e);
                    return Err(e);
                }
            }
        }

        trace!("Parser", "Looking for closing brace for block");
        match self.consume_advance_if(
            |t| *t == TokenType::RightBrace,
            "Expected '}' after a block",
        ) {
            Ok(_) => {
                debug!(
                    "Parser",
                    "Successfully parsed block with {} statements",
                    statements.len()
                );
                Ok(Stmt::Block { statements })
            }
            Err(e) => {
                error!("Parser", "Failed to find closing brace: {}", e);
                Err(e)
            }
        }
    }

    fn if_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parser", "Parsing if statement condition");
        let condition = match self.expression() {
            Ok(expr) => {
                debug!("Parser", "Successfully parsed if condition");
                expr
            }
            Err(e) => {
                error!("Parser", "Failed to parse if condition: {}", e);
                return Err(e);
            }
        };

        trace!("Parser", "Looking for opening brace for if body");
        if self.match_token(TokenType::LeftBrace) {
            debug!("Parser", "Parsing if statement body");
            let then_branch = match self.block() {
                Ok(block) => {
                    debug!("Parser", "Successfully parsed if body");
                    block
                }
                Err(e) => {
                    error!("Parser", "Failed to parse if body: {}", e);
                    return Err(e);
                }
            };

            trace!("Parser", "Checking for else clause");
            if self.match_token(TokenType::Else) {
                debug!("Parser", "Parsing else clause");

                trace!("Parser", "Looking for opening brace for else body");
                if self.match_token(TokenType::LeftBrace) {
                    debug!("Parser", "Parsing else body");
                    let else_branch = match self.block() {
                        Ok(block) => {
                            debug!("Parser", "Successfully parsed else body");
                            block
                        }
                        Err(e) => {
                            error!("Parser", "Failed to parse else body: {}", e);
                            return Err(e);
                        }
                    };

                    debug!("Parser", "Successfully parsed complete if-else statement");
                    Ok(Stmt::If {
                        condition,
                        then_branch: Box::new(then_branch),
                        else_branch: Some(Box::new(else_branch)),
                    })
                } else {
                    error!("Parser", "Expected {{ after else, found {:?}", self.peek());
                    Err(String::from("Expected '{' after condition"))
                }
            } else {
                debug!("Parser", "Successfully parsed if statement without else");
                Ok(Stmt::If {
                    condition,
                    then_branch: Box::new(then_branch),
                    else_branch: None,
                })
            }
        } else {
            error!(
                "Parser",
                "Expected {{ after if condition, found {:?}",
                self.peek()
            );
            Err(String::from("Expected '{' after condition"))
        }
    }

    fn echo_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parser", "Parsing echo statement expression");
        match self.expression() {
            Ok(expr) => {
                debug!("Parser", "Successfully parsed echo statement");
                Ok(Stmt::Echo { expression: expr })
            }
            Err(e) => {
                error!("Parser", "Failed to parse echo expression: {}", e);
                Err(e)
            }
        }
    }

    fn equality(&mut self) -> Result<Expr, String> {
        trace!("Parser", "Parsing equality expression");
        let mut expr = match self.comparison() {
            Ok(e) => e,
            Err(e) => {
                error!("Parser", "Failed to parse left side of equality: {}", e);
                return Err(e);
            }
        };

        while self.match_tokens(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let operator = self.previous().clone();
            trace!(
                "Parser",
                "Found equality operator: {:?}",
                operator.token_type
            );

            let right_expr = match self.term() {
                Ok(e) => e,
                Err(e) => {
                    error!("Parser", "Failed to parse right side of equality: {}", e);
                    return Err(e);
                }
            };

            trace!(
                "Parser",
                "Creating binary expression with operator {:?}",
                operator.token_type
            );
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right_expr),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, String> {
        trace!("Parser", "Parsing comparison expression");
        let mut expr = match self.term() {
            Ok(e) => e,
            Err(e) => {
                error!("Parser", "Failed to parse left side of comparison: {}", e);
                return Err(e);
            }
        };

        while self.match_tokens(&[
            TokenType::Greater,
            TokenType::GreaterOrEq,
            TokenType::Less,
            TokenType::LessOrEq,
        ]) {
            let operator = self.previous().clone();
            trace!(
                "Parser",
                "Found comparison operator: {:?}",
                operator.token_type
            );

            let right_expr = match self.term() {
                Ok(e) => e,
                Err(e) => {
                    error!("Parser", "Failed to parse right side of comparison: {}", e);
                    return Err(e);
                }
            };

            trace!(
                "Parser",
                "Creating binary expression with operator {:?}",
                operator.token_type
            );
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right_expr),
            }
        }

        Ok(expr)
    }

    fn expression_statement(&mut self) -> Result<Stmt, String> {
        debug!("Parser", "Parsing expression statement");
        match self.expression() {
            Ok(expr) => {
                debug!("Parser", "Successfully parsed expression statement");
                Ok(Stmt::Expression { expression: expr })
            }
            Err(e) => {
                error!("Parser", "Failed to parse expression statement: {}", e);
                Err(e)
            }
        }
    }

    fn expression(&mut self) -> Result<Expr, String> {
        trace!("Parser", "Parsing expression");
        self.equality()
    }

    fn term(&mut self) -> Result<Expr, String> {
        trace!("Parser", "Parsing term");
        let mut expr = match self.factor() {
            Ok(e) => e,
            Err(e) => {
                error!("Parser", "Failed to parse term: {}", e);
                return Err(e);
            }
        };

        while self.match_tokens(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous().clone();
            trace!("Parser", "Found term operator: {:?}", operator.token_type);

            let right = match self.factor() {
                Ok(e) => e,
                Err(e) => {
                    error!("Parser", "Failed to parse right side of term: {}", e);
                    return Err(e);
                }
            };

            trace!(
                "Parser",
                "Creating binary expression with operator {:?}",
                operator.token_type
            );
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, String> {
        trace!("Parser", "Parsing factor");
        let mut expr = match self.unary() {
            Ok(e) => e,
            Err(e) => {
                error!("Parser", "Failed to parse factor: {}", e);
                return Err(e);
            }
        };

        while self.match_tokens(&[TokenType::Star, TokenType::Slash]) {
            let operator = self.previous().clone();
            trace!("Parser", "Found factor operator: {:?}", operator.token_type);

            let right = match self.unary() {
                Ok(e) => e,
                Err(e) => {
                    error!("Parser", "Failed to parse right side of factor: {}", e);
                    return Err(e);
                }
            };

            trace!(
                "Parser",
                "Creating binary expression with operator {:?}",
                operator.token_type
            );
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        trace!("Parser", "Parsing unary expression");

        if self.match_tokens(&[TokenType::Minus]) {
            let operator = self.previous().clone();
            trace!("Parser", "Found unary operator: {:?}", operator.token_type);

            let right = match self.unary() {
                Ok(e) => e,
                Err(e) => {
                    error!("Parser", "Failed to parse unary expression: {}", e);
                    return Err(e);
                }
            };

            trace!(
                "Parser",
                "Creating unary expression with operator {:?}",
                operator.token_type
            );
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, String> {
        trace!("Parser", "Parsing primary expression");

        if self.match_token(TokenType::LeftParen) {
            debug!("Parser", "Parsing grouped expression");
            let expr = match self.expression() {
                Ok(e) => e,
                Err(e) => {
                    error!("Parser", "Failed to parse grouped expression: {}", e);
                    return Err(e);
                }
            };

            trace!("Parser", "Looking for closing parenthesis");
            match self.consume_advance_if(
                |t| *t == TokenType::RightParen,
                "Expect ')' after expression.",
            ) {
                Ok(_) => {
                    debug!("Parser", "Successfully parsed grouped expression");
                    return Ok(Expr::Grouping {
                        expression: Box::new(expr),
                    });
                }
                Err(e) => {
                    error!("Parser", "Failed to find closing parenthesis: {}", e);
                    return Err(e);
                }
            }
        }

        if let Ok(token) =
            self.consume_advance_if(|t| matches!(t, TokenType::Number(_)), "Expect expression.")
        {
            if let TokenType::Number(value) = token.token_type {
                debug!("Parser", "Parsed number literal: {}", value);
                return Ok(Expr::Literal {
                    value: LiteralValue::Number(value),
                });
            }
        }

        if let Ok(token) =
            self.consume_advance_if(|t| matches!(t, TokenType::Bool(_)), "Expect expression.")
        {
            if let TokenType::Bool(val) = token.token_type {
                debug!("Parser", "Parsed boolean literal: {}", val);
                return Ok(Expr::Literal {
                    value: LiteralValue::Bool(val),
                });
            }
        }

        if let Ok(token) =
            self.consume_advance_if(|t| matches!(t, TokenType::String(_)), "Expect expression.")
        {
            if let TokenType::String(val) = token.token_type {
                debug!("Parser", "Parsed string literal: \"{}\"", val);
                return Ok(Expr::Literal {
                    value: LiteralValue::String(val),
                });
            }
        }

        if let Ok(token) = self.consume_advance_if(
            |t| matches!(t, TokenType::Identifier(_)),
            "Expect expression.",
        ) {
            if let TokenType::Identifier(ref name) = token.token_type {
                debug!("Parser", "Parsed variable reference: {}", name);
            }
            return Ok(Expr::Variable { name: token });
        }

        error!(
            "Parser",
            "Unexpected token: {:?} at line {}, column {}",
            self.peek().token_type,
            self.peek().line,
            self.peek().column
        );
        Err(format!("Unexpected token: {:?}", self.peek()))
    }

    // Helper methods
    fn match_token(&mut self, token_type: TokenType) -> bool {
        trace!("Parser", "Matching against token type: {:?}", token_type);
        if self.check(|t| *t == token_type) {
            self.advance();
            trace!("Parser", "Token matched");
            true
        } else {
            trace!(
                "Parser",
                "Token did not match, found: {:?}",
                self.peek().token_type
            );
            false
        }
    }

    fn match_tokens(&mut self, token_types: &[TokenType]) -> bool {
        trace!("Parser", "Matching against multiple token types");
        for token_type in token_types {
            if self.check(|t| t == token_type) {
                self.advance();
                trace!("Parser", "Token matched: {:?}", token_type);
                return true;
            }
        }

        trace!(
            "Parser",
            "No token matched, found: {:?}",
            self.peek().token_type
        );
        false
    }

    /// Basically skips token if predicate is true
    /// If predicate returns true -> we skip to next token and return the old one.
    /// if returns false -> errors
    fn consume_advance_if<F>(&mut self, predicate: F, error_message: &str) -> Result<Token, String>
    where
        F: Fn(&TokenType) -> bool,
    {
        trace!(
            "Parser",
            "Advancing if predicate matches, current token: {:?}",
            self.peek().token_type
        );
        if self.check(&predicate) {
            let token = self.advance();
            trace!("Parser", "Token consumed: {:?}", token.token_type);
            Ok(token)
        } else {
            let err = format!("{} Got {:?}", error_message, self.peek());
            trace!("Parser", "Token consumption failed: {}", err);
            Err(err)
        }
    }

    fn check<F>(&self, predicate: F) -> bool
    where
        F: Fn(&TokenType) -> bool,
    {
        if self.is_at_end() {
            trace!("Parser", "Check failed: at end of input");
            false
        } else {
            predicate(&self.peek().token_type)
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
            trace!(
                "Parser",
                "Advanced to token {}: {:?}",
                self.current - 1,
                self.previous().token_type
            );
        } else {
            trace!("Parser", "Attempted to advance past end of input");
        }

        self.previous().clone()
    }

    fn is_at_end(&self) -> bool {
        let at_end = matches!(self.peek().token_type, TokenType::EOF);
        at_end
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}
