use crate::ast;
use crate::lexer;
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

    pub fn parse(&mut self) -> Expr {
        self.expression()
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }


    fn equality(&mut self) -> Expr {
        let mut left = self.comparison();
        loop {
            match self.current_token().token_type {
                TokenType::EqualEqual | TokenType::BangEqual => {
                    let operator = self.current_token().clone();
                    self.consume();
                    let right = self.comparison();
                    left = Expr::new_binary_expr(Binary::new(operator, left, right));
                },
                _ => break,
            }
        }
        return left
    }

    fn comparison(&mut self) -> Expr {
        let mut left = self.addition();
        loop {
            match self.current_token().token_type {
                TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual => {
                    let operator = self.current_token().clone();
                    self.consume();
                    let right = self.addition();
                    left = Expr::new_binary_expr(Binary::new(operator, left, right));
                },
                _ => break,
            }
        }
        return left;
    }

    fn addition(&mut self) -> Expr {
        let mut expr = self.multiplication();
        loop {
            match self.current_token().token_type {
                TokenType::Plus | TokenType::Minus | TokenType::PlusPlus => {
                    let operator = self.current_token().clone();
                    self.consume();
                    let right = self.multiplication();
                    expr = Expr::new_binary_expr(Binary::new(operator, expr , right));    
                },
                _ => break,
            }
        }
        expr
    }

    fn multiplication(&mut self) -> Expr {
        let mut expr = self.unary();
        loop {
            match self.current_token().token_type {
                TokenType::Star | TokenType::Slash => {
                    let operator = self.current_token().clone();
                    self.consume();
                    let right = self.unary();
                    expr = Expr::new_binary_expr(Binary::new(operator, expr, right))
                },
                _ => break,
            }
        }
        expr
    }


    fn unary(&mut self) -> Expr {
        match self.current_token().token_type {
            TokenType::Minus | TokenType::Bang => {
                let operator = self.current_token().clone();
                self.consume();
                let expr = self.unary();
                Expr::new_unary_expr(Unary::new(operator, expr))
            },
            _ => self.literal(),
        }   
    }

    fn literal(&mut self) -> Expr {
        match self.current_token().token_type {
            TokenType::NUMBER => {
                let e = Expr::new_literal_expr(Literal::new(self.current_token().lexeme.clone()));
                self.consume();
                e
            },
            TokenType::STRING => {
                let e = Expr::new_literal_expr(Literal::new(self.current_token().lexeme.clone()));
                self.consume();
                e
            },
            TokenType::TRUE | TokenType::FALSE => {
                let e = Expr::new_literal_expr(Literal::new(self.current_token().lexeme.clone()));
                self.consume();
                e
            },
            TokenType::Nil => {
                let e = Expr::new_literal_expr(Literal::new(self.current_token().lexeme.clone()));
                self.consume();
                e
            },
            TokenType::LeftParen => {
                self.consume();
                let expr = self.expression();
                match self.consume_right_paren() {
                    Ok(_) => {
                        Expr::new_grouping_expr(Grouping::new(expr))
                    },
                    Err(_) => panic!("Expected )"),
                }
            },

            _ => panic!("Invalid literal"),
        }
    }



    fn consume_right_paren(&mut self) -> Result<Token, ()> {
        if TokenType::RightParen == self.current_token().token_type {
            self.consume();
            return Ok(self.current_token().clone())
        }
        Err(())
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