use crate::ast::{Binary, Unary, Literal, Grouping, Expr, Stmt};
use crate::errors::{RuntimeError};
use crate::lexer::{TokenType, Token};
use crate::environment::{ Environment };
use std::fmt;

pub struct Interpreter {
    environment: Environment
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { environment: Environment::new() }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), RuntimeError> {
        for statement in statements {
            match statement {
                Stmt::ExprStmt(expr) => {
                    expr.visit_expr(&expr)?;
                },
                Stmt::PrintStmt(expr) => {
                    let e = expr.visit_expr(&expr)?;
                    println!("{}", e);
                },
            }
        }
        Ok(())
    }
}



/// Implement a Visitor for each struct in the Abstract Syntax Tree
pub trait Visitor<E>  {
    fn accept<R, V: Interp<R>>(&self, visitor: &V) -> Result<R, E>;
}

impl Visitor<RuntimeError> for Unary {
    fn accept<R, V: Interp<R>>(&self, visitor: &V) -> Result<R, RuntimeError> {
        visitor.visit_unary(self)
    }
}

impl Visitor<RuntimeError> for Literal {
    fn accept<R, V: Interp<R>>(&self, visitor: &V) -> Result<R, RuntimeError> {
        visitor.visit_literal(self)
    }
}

impl Visitor<RuntimeError> for Binary {
    fn accept<R, V: Interp<R>>(&self, visitor: &V) -> Result<R, RuntimeError> {
        visitor.visit_binary(self)
    }
}

impl Visitor<RuntimeError> for Grouping {
    fn accept<R, V: Interp<R>>(&self, visitor: &V) -> Result<R, RuntimeError> {
        visitor.visit_grouping(self)
    }
}

impl Visitor<RuntimeError> for Stmt {
    fn accept<R, V: Interp<R>>(&self, visitor: &V) -> Result<R, RuntimeError> {
       visitor.visit_stmt(self)
    }
}

impl Visitor<RuntimeError> for Expr {
    fn accept<R, V: Interp<R>>(&self, visitor: &V) -> Result<R, RuntimeError> {
        visitor.visit_expr(self)
    }
}



pub trait Interp<R> {
    fn visit_binary(&self, binary: &Binary) -> Result<R, RuntimeError>;
    fn visit_unary(&self, unary: &Unary) -> Result<R, RuntimeError>;
    fn visit_literal(&self, literal: &Literal) -> Result<R, RuntimeError>;
    fn visit_grouping(&self, grouping: &Grouping) -> Result<R, RuntimeError>;
    fn visit_stmt(&self, stmt: &Stmt) -> Result<R, RuntimeError>;
    fn visit_expr(&self, expr: &Expr) -> Result<R, RuntimeError>;
}



pub fn interpret_ast_expr(expression: Expr) -> Result<Value, RuntimeError> {
    match expression {
        Expr::B(ref val) => expression.visit_binary(val),
        Expr::G(ref val) => expression.visit_grouping(val),
        Expr::L(ref val) => expression.visit_literal(val),
        Expr::U(ref val) => expression.visit_unary(val),
    }
}

macro_rules! evaluate {
    ($e:expr, $sel:ident) => {
        match &$e {
            Expr::L(lit) => lit.accept($sel),
            Expr::B(ref b_expr) => b_expr.accept($sel),
            Expr::U(ref u_expr) => u_expr.accept($sel),
            Expr::G(ref g_expr) => g_expr.accept($sel),
        }?;
    }
}

#[derive(PartialEq, Debug)]
pub enum Value {
    BOOL(bool),
    STRING(String),
    NUMBER(f64),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        match self {
            Value::BOOL(val) => write!(f, "{}", val),
            Value::Nil => write!(f, "nil"),
            Value::STRING(val) => write!(f, "\"{}\"", val),
            Value::NUMBER(val) => write!(f, "{}", val),
        }
    }
}

impl Interp<Value> for Expr {
    fn visit_binary(&self, binary: &Binary) -> Result<Value, RuntimeError> {


        let right: Value = evaluate!(binary.right, self);
        let left: Value = evaluate!(binary.left, self);
        
        match binary.operator.token_type {
            TokenType::Minus => check_numbers((left, right), &binary.operator),
            TokenType::Plus => check_numbers((left, right), &binary.operator),
            TokenType::Star => check_numbers((left, right), &binary.operator),
            TokenType::Slash => check_numbers((left, right), &binary.operator),
            TokenType::PlusPlus => concatenate_values((left, right), &binary.operator),
            TokenType::EqualEqual => determine_equality((left, right), &binary.operator),
            TokenType::BangEqual => determine_equality((left, right), &binary.operator),
            TokenType::Less => determine_int_comparison((left, right), &binary.operator),
            TokenType::LessEqual => determine_int_comparison((left, right), &binary.operator),
            TokenType::Greater => determine_int_comparison((left, right), &binary.operator),
            TokenType::GreaterEqual => determine_int_comparison((left, right), &binary.operator),
            _ => Err(RuntimeError::new(binary.operator.lexeme.clone(), format!("Expected expression, given {}", binary.operator.lexeme), binary.operator.line)),
        }
    }

    fn visit_unary(&self, unary: &Unary) -> Result<Value, RuntimeError> {
        let expr: Value = evaluate!(unary.expr, self);

        match unary.operator.token_type {
            TokenType::Minus => {
                if let Value::NUMBER(v) = expr {
                   return Ok(Value::NUMBER(-1.0 * v));
                }
                Err(RuntimeError::new(unary.operator.lexeme.clone(), "Invalid unary expression.  Expected Number".to_string(), unary.operator.line))
            },
            TokenType::Bang => Ok(Value::BOOL(!is_truthy(expr))),
            _ => Err(RuntimeError::new(unary.operator.lexeme.clone(), "Invalid token for Unary".to_string(), unary.operator.line)),
        }
    }

    fn visit_literal(&self, literal: &Literal) -> Result<Value, RuntimeError> {        
        if literal.val.parse::<f64>().is_ok() {
            Ok(Value::NUMBER(literal.val.parse::<f64>().unwrap()))
        }
        else if literal.val.parse::<bool>().is_ok() && literal.val != "true" && literal.val != "false" {
            Ok(Value::BOOL(literal.val.parse::<bool>().unwrap()))
        }
        else if literal.val.parse::<String>().is_ok() {
            let s = literal.val.parse::<String>().unwrap();
            match &s[..] {
                "nil" => Ok(Value::Nil),
                "true" => Ok(Value::BOOL(true)),
                "false" => Ok(Value::BOOL(false)),
                _ => Ok(Value::STRING(s)),
            }
        }
        else {
            Err(RuntimeError::new(String::new(), format!("Invalid literal value, given: {}", literal.val), 1)) //TODO:Better Error handling
        }
    }

    fn visit_grouping(&self, grouping: &Grouping) -> Result<Value, RuntimeError> {
        Ok(evaluate!(grouping.expr, self))
    }

    fn visit_stmt(&self, stmt: &Stmt) -> Result<Value, RuntimeError> {
        match stmt {
            Stmt::ExprStmt(expr) => self.visit_expr(expr),
            Stmt::PrintStmt(expr) => self.visit_expr(expr),
        }
    }
    fn visit_expr(&self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::L(e) => self.visit_literal(e),
            Expr::U(e) => self.visit_unary(e),
            Expr::G(e) => self.visit_grouping(e),
            Expr::B(e) => self.visit_binary(e),
        }
    }
}


fn check_numbers(paris: (Value, Value), op: &Token) -> Result<Value, RuntimeError> {
    match paris {
        (Value::NUMBER(left), Value::NUMBER(right)) => {
            match op.token_type {
                TokenType::Minus => Ok(Value::NUMBER(left - right)),
                TokenType::Plus => Ok(Value::NUMBER(left + right)),
                TokenType::Star => Ok(Value::NUMBER(left * right)),
                TokenType::Slash => Ok(Value::NUMBER(left / right)),
                _ => Err(RuntimeError::new(op.lexeme.clone(), format!("Invalid binary operator for numbers, given {}", op.lexeme), op.line)),
            }
        }
        _ => Err(RuntimeError::new(op.lexeme.clone(), format!("'{}' can only be applied to numbers, given: {}, {}", op.lexeme.clone(), paris.0, paris.1), op.line)),
    }
}

// Two cases:
// left and right are strings               =>combine the strings 
// left is a string and right is a int      => combine the string and int into a string
fn concatenate_values(pairs: (Value, Value), token: &Token) -> Result<Value, RuntimeError> {
    match pairs {
        (Value::STRING(mut v), Value::STRING(v2)) => {
            v.push_str(&v2);
            Ok(Value::STRING(v))
        },
        (Value::STRING(mut v), Value::NUMBER(v2)) => {
            v.push_str(&v2.to_string());
            Ok(Value::STRING(v))
        }
        (Value::NUMBER(v), Value::STRING(v2)) => {
            let mut s = v.to_string();
            s.push_str(&v2);
            Ok(Value::STRING(s))
        },
        _ => Err(RuntimeError::new(token.lexeme.clone(), format!("'{}' can only be applied to String and Numbers, given: {}, {}", token.lexeme, pairs.0, pairs.1), token.line)),
    }
}


fn determine_equality(pair: (Value, Value), token: &Token) -> Result<Value, RuntimeError> {
    match token.token_type {
        TokenType::EqualEqual => {
             match pair {
                (Value::BOOL(v), Value::BOOL(v2)) => Ok(Value::BOOL(v == v2)),
                (Value::Nil, Value::Nil) => Ok(Value::BOOL(true)),
                (Value::STRING(v), Value::STRING(v2)) => Ok(Value::BOOL(v == v2)),
                (Value::NUMBER(v), Value::NUMBER(v2)) => Ok(Value::BOOL(v == v2)),
                _ => Ok(Value::BOOL(false)),
            }
        },
        TokenType::BangEqual => {
            match pair {
                (Value::BOOL(v), Value::BOOL(v2)) =>Ok(Value::BOOL(v != v2)),
                (Value::Nil, Value::Nil) => Ok(Value::BOOL(false)),
                (Value::STRING(v), Value::STRING(v2)) => Ok(Value::BOOL(v != v2)),
                (Value::NUMBER(v), Value::NUMBER(v2)) => Ok(Value::BOOL(v != v2)),
                _ => Ok(Value::BOOL(true)),
            }
        },
        _ => Err(RuntimeError::new(token.lexeme.clone(), "Invalid token type. Expected '==' or '!='.".to_string(), token.line)),
    }
}

fn determine_int_comparison(pair: (Value, Value), token: &Token) -> Result<Value, RuntimeError> {
    match pair {
        (Value::NUMBER(val), Value::NUMBER(val2)) => {
            match token.token_type {
                TokenType::Less => Ok(Value::BOOL(val < val2)),
                TokenType::LessEqual => Ok(Value::BOOL(val <= val2)),
                TokenType::Greater => Ok(Value::BOOL(val > val2)),
                TokenType::GreaterEqual => Ok(Value::BOOL(val >= val2)),
                _ => panic!("Expected boolean values")
            } 
        }, 
        _ => Err(RuntimeError::new(token.lexeme.clone(), "Expected two numbers.".to_string(), token.line)),
    }
}

// Determines if  a value is truthy or falsy
// Important: Flax follows Ruby's rule: everything but False and nil are true
fn is_truthy(value: Value) -> bool {
    match value {
        Value::BOOL(false) | Value::Nil => false,
        _ => true,
    }
}