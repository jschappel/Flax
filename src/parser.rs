use crate::ast;
use crate::lexer;
use crate::errors;
use errors::ParseError;
use ast::{ Binary, Unary, Literal, Expr, Grouping };
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
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, index: 0 }
    }

    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }


    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut left: Expr = self.comparison()?;
        loop {
            match self.current_token().token_type {
                TokenType::EqualEqual | TokenType::BangEqual => {
                    let operator = self.current_token().clone();
                    self.consume();
                    let right = self.comparison()?;
                    left = Expr::new_binary_expr(Binary::new(operator, left, right));
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
                    left = Expr::new_binary_expr(Binary::new(operator, left, right));
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
                    expr = Expr::new_binary_expr(Binary::new(operator, expr , right));    
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
                    expr = Expr::new_binary_expr(Binary::new(operator, expr, right))
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
                Ok(Expr::new_unary_expr(Unary::new(operator, expr)))
            },
            _ => self.literal(),
        }   
    }


    fn literal(&mut self) -> Result<Expr, ParseError> {
        let token = self.current_token();
        match token.token_type {
            TokenType::NUMBER => {
                let e = Expr::new_literal_expr(Literal::new(token.lexeme.clone()));
                self.consume();
                Ok(e)
            },
            TokenType::STRING => {
                let e = Expr::new_literal_expr(Literal::new(token.lexeme.clone()));
                self.consume();
                Ok(e)
            },
            TokenType::TRUE | TokenType::FALSE => {
                let e = Expr::new_literal_expr(Literal::new(token.lexeme.clone()));
                self.consume();
                Ok(e)
            },
            TokenType::Nil => {
                let e = Expr::new_literal_expr(Literal::new(token.lexeme.clone()));
                self.consume();
                Ok(e)
            },
            // Error handling cases below
            TokenType::LeftParen => {
                self.consume();
                let expr: Expr = self.expression()?;
                match self.consume_right_paren() {
                    Ok(_) => Ok(Expr::new_grouping_expr(Grouping::new(expr))),
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

/*
    EqualEqual, Bang, BangEqual,
*/

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
}

pub mod test {
    
}