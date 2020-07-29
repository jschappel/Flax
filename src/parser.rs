use crate::ast;
use crate::lexer;
use crate::errors;
use errors::ParseError;
use ast::{Expr, Stmt};
use lexer::{ Token, TokenType };
/** Precedence      Operators           Associates
 * unary               -                   right
 * multiplication      * /                  left
 * addition            + -                  left 
 * 
 * 
 * expression       => equality
 * equality         => comparison ( ( '==' | '!=' ) comparison )*
 * comparison       => addition ( ('>' | '<' | '>=' '<=' ) addition )*
 * addition         => multiplication ( ('+' | '-') multiplication )*
 * multiplication   => unary ( ('*' | '/') unary )*
 * unary           => ('-') unary
 *                    | primary
 * literal          => NUMBER | STRING | true | false | nil
 *                    | "(" expression ")"
 * 
**/
pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    loops: u32,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, index: 0, loops: 0}
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?)
        }
        Ok(statements)
    }

    pub fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.expression()
    }

    //TODO: synchronize the parser here
    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        match self.current_token().token_type {
            TokenType::Let => {
                self.consume();
                self.var_declaration()
            },
            _ => self.statement(),
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let identifier = self.check_and_consume(TokenType::Identifier, "Expected variable name")?;
        let mut initializer = None;
        if self.current_token().token_type == TokenType::Equal {
            self.consume();
            initializer = Some(self.expression()?);
        }
        self.check_and_consume(TokenType::Semicolon, "Expected ';' after variable declaration")?;
        Ok(Stmt::VarDecl(identifier, initializer))
    }


    fn statement(&mut self) -> Result<Stmt, ParseError> {
        match self.current_token().token_type {
            TokenType::Func => {self.consume(); self.function("function")},
            TokenType::If => self.if_statement(),
            TokenType::LeftBrace => {self.consume(); self.make_block()},
            TokenType::While => self.while_stmt(),
            TokenType::Return => self.return_stmt(),
            TokenType::Break => self.break_statement(),
            _ => self.expression_statement(),
        }
    }

    fn return_stmt(&mut self) -> Result<Stmt, ParseError> {
        let token = self.current_token().clone();
        self.consume(); // eat the return token
        let mut expr = None;

        if self.current_token().token_type != TokenType::Semicolon {
            expr = Some(self.expression()?); 
        }

        self.check_and_consume(TokenType::Semicolon, "Expected ';' after return value")?;
        Ok(Stmt::new_return(token, expr))
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, ParseError> {
        let name = self.check_and_consume(TokenType::Identifier, &format!("Expect {} name.", kind))?;
        self.check_and_consume(TokenType::LeftParen, &format!("Expect '(' after {} name.", kind))?;
        let mut parameters = Vec::new();
        if self.current_token().token_type != TokenType::RightParen {
            loop {
                if parameters.len() > 127 {
                    return Err(ParseError::new("Cannot have more then 127 arguments.".to_string(),self.current_token().line))
                }
                parameters.push(self.check_and_consume(TokenType::Identifier, "Expected parameter name")?);
            
                if self.current_token().token_type != TokenType::Comma {
                    break;
                }
                self.consume(); // eat the ','
            }
        }
        self.check_and_consume(TokenType::RightParen, "Expected ')' after parameters")?;
        self.check_and_consume(TokenType::LeftBrace, "Expected '{' after function declaration")?;
        let body = self.block()?;

        Ok(Stmt::new_function(name, parameters, body))
    }

    fn break_statement(&mut self) -> Result<Stmt, ParseError> {
        if self.loops > 0 {
            self.consume();
            self.check_and_consume(TokenType::Semicolon, "Expected ';' after statement")?;
            return Ok(Stmt::Break)
        }
        Err(ParseError::new("'break' can only be used inside a while loop".to_string(), self.current_token().line))
    }

    fn while_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.consume(); // consume the while token
        let condition = self.expression()?;
        self.check_and_consume(TokenType::LeftBrace, "Expected '{' after while condition")?;
        self.loops +=1; // Increment the loop counter
        let block = self.make_block()?;
        self.loops -=1; // Decrease the loop counter after all statements have been parsed
        Ok(Stmt::new_while(condition, block))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(); // consume the if Stmt
        let expr: Expr = self.expression()?;
        self.check_and_consume(TokenType::LeftBrace, "Expected block expression after a if expression")?;
        let then_block = self.make_block()?;

        if self.current_token().token_type == TokenType::Else {
            self.consume(); // consume the else
            self.check_and_consume(TokenType::LeftBrace, "Expected block expression after a else expression")?;
            let else_block = self.make_block()?;
            return Ok(Stmt::new_if(expr, then_block, Some(else_block)));
        }

        Ok(Stmt::new_if(expr, then_block, None))
    }


    fn make_block(&mut self) -> Result<Stmt, ParseError>  {
        let statements = self.block()?;
        Ok(Stmt::new_block(statements))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();
        while self.current_token().token_type != TokenType::RightBrace && !self.is_at_end() {
            let statement = self.declaration()?;
            statements.push(statement)
        }

        self.check_and_consume(TokenType::RightBrace, "Expected '}'")?;
        Ok(statements)
    }


    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr: Expr = self.expression()?;
        match self.current_token().token_type {
            TokenType::Semicolon => {
                self.consume();
                Ok(Stmt::ExprStmt(expr))
            },
            _ => Err(ParseError::new("Expected ';'".to_string(), self.current_token().line)),
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logical_or()?;
        match self.current_token().token_type {
            TokenType::Equal => {
                self.consume();
                let value = self.assignment()?;

                if let Expr::V(tok) = expr {
                    return Ok(Expr::new_assignment(tok, value));
                }
                return Err(ParseError::new("Invalid assignment target".to_string(), self.current_token().line));
            },
            TokenType::PlusEqual | TokenType::MinusEqual => Ok(self.increment(expr)?),
            _ => Ok(expr),
        }
    }

    fn increment(&mut self, expr: Expr) -> Result<Expr, ParseError> {
        let token = match self.current_token().token_type {
            TokenType::PlusEqual => Token::new(TokenType::Plus, "+".to_string(), self.current_token().line),
            _ => Token::new(TokenType::Minus, "-".to_string(), self.current_token().line),
        };
        self.consume(); // eat += or -=
        let right = self.addition()?;
        if let Expr::V(ref tok) = expr {
            return Ok(Expr::new_assignment(tok.clone(), Expr::new_binary(expr, token, right)));
        }
        return Err(ParseError::new("Invalid assignment target. Expected Value".to_string(), self.current_token().line));
    }

    fn logical_or(&mut self) -> Result<Expr, ParseError> {
        let left: Expr = self.logical_and()?;
        if self.current_token().token_type == TokenType::Or {
            let tok = self.current_token().clone();
            self.consume(); // consume the or
            let right = self.logical_and()?;
            return Ok(Expr::new_logical(tok, left, right))
        }
        Ok(left)
    }

    fn logical_and(&mut self) -> Result<Expr, ParseError> {
        let left: Expr = self.conditional()?;
        if self.current_token().token_type == TokenType::And {
            let tok = self.current_token().clone();
            self.consume(); // consume the and
            let right = self.conditional()?;
            return Ok(Expr::new_logical(tok, left, right))
        }
        Ok(left)
    }

    fn conditional(&mut self) -> Result<Expr, ParseError> {
        let expr: Expr = self.equality()?;

        if self.current_token().token_type == TokenType::Question {
            self.consume(); // consume the '?'
            let then_expr = self.expression()?;
            if self.current_token().token_type == TokenType::Colon {
                self.consume(); // consume the ':'
                let else_expr = self.conditional()?;
                return Ok(Expr::new_conditional(expr, then_expr, else_expr, self.current_token().line))
            }
            return Err(ParseError::new("Expected : after then expression".to_string(), self.current_token().line));
        }
        Ok(expr)
    }


    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut left: Expr = self.comparison()?;
        loop {
            match self.current_token().token_type {
                TokenType::EqualEqual | TokenType::BangEqual => {
                    let operator = self.current_token().clone();
                    self.consume();
                    let right = self.comparison()?;
                    left = Expr::new_binary(left, operator, right);
                },
                _ => break,
            }
        }
        Ok(left)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut left: Expr = self.addition()?;
        loop {
            match self.current_token().token_type {
                TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual => {
                    let operator = self.current_token().clone();
                    self.consume();
                    let right = self.addition()?;
                    left = Expr::new_binary(left, operator, right);
                },
                _ => break,
            }
        }
        Ok(left)
    }

    fn addition(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.multiplication()?;
        loop {
            match self.current_token().token_type {
                TokenType::Plus | TokenType::Minus | TokenType::PlusPlus => {
                    let operator = self.current_token().clone();
                    self.consume();
                    let right = self.multiplication()?;
                    expr = Expr::new_binary(expr, operator, right);    
                },
                _ => break,
            }
        }
        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.unary()?;
        loop {
            match self.current_token().token_type {
                TokenType::Star | TokenType::Slash => {
                    let operator = self.current_token().clone();
                    self.consume();
                    let right = self.unary()?;
                    expr = Expr::new_binary(expr, operator, right);
                },
                _ => break,
            }
        }
        Ok(expr)
    }


    fn unary(&mut self) -> Result<Expr, ParseError> {
        match self.current_token().token_type {
            TokenType::Minus | TokenType::Bang => {
                let operator = self.current_token().clone();
                self.consume();
                let expr = self.unary()?;
                Ok(Expr::new_unary(operator, expr))
            },
            _ => self.call(),
        }   
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.literal()?;
        loop {
            if self.current_token().token_type == TokenType::LeftParen {
                self.consume(); // eat the '('
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn literal(&mut self) -> Result<Expr, ParseError> {
        let token = self.current_token();
        match token.token_type {
            TokenType::NUMBER => {
                let e = Expr::new_literal(token.lexeme.clone());
                self.consume();
                Ok(e)
            },
            TokenType::STRING => {
                let e = Expr::new_literal(token.lexeme.clone());
                self.consume();
                Ok(e)
            },
            TokenType::TRUE | TokenType::FALSE => {
                let e = Expr::new_literal(token.lexeme.clone());
                self.consume();
                Ok(e)
            },
            TokenType::Nil => {
                let e = Expr::new_literal(token.lexeme.clone());
                self.consume();
                Ok(e)
            },
            TokenType::Identifier => {
                let e = Expr::new_variable(token.clone());
                self.consume();
                Ok(e)
            },
            // Error handling cases below
            TokenType::LeftParen => {
                self.consume();
                let expr: Expr = self.expression()?;
                match self.consume_right_paren() {
                    Ok(_) => Ok(Expr::new_grouping(expr)),
                    Err(lexeme) => Err(ParseError::new(format!("Expected ')' given {}", lexeme), self.current_token().line)),
                }
            },
            TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Slash | TokenType::Greater
            | TokenType::Less | TokenType::LessEqual | TokenType::GreaterEqual => {
                Err(ParseError::new(format!("Expected number before {}", token.lexeme), token.line))
            },
            TokenType::PlusPlus => Err(ParseError::new(format!("Expected number of string before {}", token.lexeme), token.line)),
            TokenType::EqualEqual | TokenType::BangEqual => {
                Err(ParseError::new(format!("Expected expression before {}", token.lexeme), token.line))
            },
            _ => Err(ParseError::new("Expected expression".to_string(), token.line)),
        }
    }


    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut arguments: Vec<Expr> = Vec::new();
        if self.current_token().token_type != TokenType::RightParen {
            loop{
                arguments.push(self.expression()?);
                
                if arguments.len() > 127 {
                    return Err(ParseError::new("Cannot have more then 127 arguments.".to_string(),self.current_token().line))
                }
                if self.current_token().token_type != TokenType::Comma {
                    break;
                }
                self.consume(); // eat the ','
            }
        }
        let paren = self.check_and_consume(TokenType::RightParen, "Expected ')' after arguments.")?;
    
        Ok(Expr::new_call(callee, paren, arguments))
    }


    fn consume_right_paren(&mut self) -> Result<Token, &str> {
        if TokenType::RightParen == self.current_token().token_type {
            self.consume();
            return Ok(self.current_token().clone())
        }
        Err(&self.current_token().lexeme)
    }


    fn current_token(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn consume(&mut self) {
        if self.current_token().token_type != TokenType::EOF {
            self.index += 1;
        }
    }

    fn check_and_consume(&mut self, tok_type: TokenType, message: &str) -> Result<Token, ParseError> {
        if self.current_token().token_type == tok_type {
            let token = self.current_token().clone();
            self.consume();
            return Ok(token)
        }
        return Err(ParseError::new(message.to_string(), self.current_token().line));
    }

    fn is_at_end(&mut self) -> bool { 
        self.current_token().token_type == TokenType::EOF
    }
}