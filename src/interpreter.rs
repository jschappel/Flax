use crate::ast::{Binary, Unary, Literal, Grouping, Expr};
use crate::errors::{RuntimeError};
use crate::lexer::{TokenType, Token};
use std::fmt;


/// Implement a Visitor for each struct in the Abstract Syntax Tree
pub trait Visitor<E>  {
    fn accept<R, V: Interpreter<R>>(&self, visitor: &V) -> Result<R, E>;
}

impl Visitor<RuntimeError> for Unary {
    fn accept<R, V: Interpreter<R>>(&self, visitor: &V) -> Result<R, RuntimeError> {
        visitor.visit_unary(self)
    }
}

impl Visitor<RuntimeError> for Literal {
    fn accept<R, V: Interpreter<R>>(&self, visitor: &V) -> Result<R, RuntimeError> {
        visitor.visit_literal(self)
    }
}

impl Visitor<RuntimeError> for Binary {
    fn accept<R, V: Interpreter<R>>(&self, visitor: &V) -> Result<R, RuntimeError> {
        visitor.visit_binary(self)
    }
}

impl Visitor<RuntimeError> for Grouping {
    fn accept<R, V: Interpreter<R>>(&self, visitor: &V) -> Result<R, RuntimeError> {
        visitor.visit_grouping(self)
    }
}



pub trait Interpreter<R> {

    fn visit_binary(&self, binary: &Binary) -> Result<R, RuntimeError>;
    fn visit_unary(&self, unary: &Unary) -> Result<R, RuntimeError>;
    fn visit_literal(&self, literal: &Literal) -> Result<R, RuntimeError>;
    fn visit_grouping(&self, grouping: &Grouping) -> Result<R, RuntimeError>;

}



pub fn interpret_ast(expression: Expr) -> Result<Obj, RuntimeError> {
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
pub enum Obj {
    BOOL(bool),
    STRING(String),
    NUMBER(f64),
    Nil,
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        match self {
            Obj::BOOL(val) => write!(f, "{}", val),
            Obj::Nil => write!(f, "nil"),
            Obj::STRING(val) => write!(f, "\"{}\"", val),
            Obj::NUMBER(val) => write!(f, "{}", val),
        }
    }
}

impl Interpreter<Obj> for Expr {
    fn visit_binary(&self, binary: &Binary) -> Result<Obj, RuntimeError> {


        let right: Obj = evaluate!(binary.right, self);
        let left: Obj = evaluate!(binary.left, self);
        
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

    fn visit_unary(&self, unary: &Unary) -> Result<Obj, RuntimeError> {
        let expr: Obj = evaluate!(unary.expr, self);

        match unary.operator.token_type {
            TokenType::Minus => {
                if let Obj::NUMBER(v) = expr {
                   return Ok(Obj::NUMBER(-1.0 * v));
                }
                Err(RuntimeError::new(unary.operator.lexeme.clone(), "Invalid unary expression.  Expected Number".to_string(), unary.operator.line))
            },
            TokenType::Bang => Ok(Obj::BOOL(!is_truthy(expr))),
            _ => Err(RuntimeError::new(unary.operator.lexeme.clone(), "Invalid token for Unary".to_string(), unary.operator.line)),
        }
    }

    fn visit_literal(&self, literal: &Literal) -> Result<Obj, RuntimeError> {        
        if literal.val.parse::<f64>().is_ok() {
            Ok(Obj::NUMBER(literal.val.parse::<f64>().unwrap()))
        }
        else if literal.val.parse::<bool>().is_ok() && literal.val != "true" && literal.val != "false" {
            Ok(Obj::BOOL(literal.val.parse::<bool>().unwrap()))
        }
        else if literal.val.parse::<String>().is_ok() {
            let s = literal.val.parse::<String>().unwrap();
            match &s[..] {
                "nil" => Ok(Obj::Nil),
                "true" => Ok(Obj::BOOL(true)),
                "false" => Ok(Obj::BOOL(false)),
                _ => Ok(Obj::STRING(s)),
            }
        }
        else {
            Err(RuntimeError::new(String::new(), format!("Invalid literal value, given: {}", literal.val), 1)) //TODO:Better Error handling
        }
    }

    fn visit_grouping(&self, grouping: &Grouping) -> Result<Obj, RuntimeError> {
        Ok(evaluate!(grouping.expr, self))
    }
}


fn check_numbers(paris: (Obj, Obj), op: &Token) -> Result<Obj, RuntimeError> {
    match paris {
        (Obj::NUMBER(left), Obj::NUMBER(right)) => {
            match op.token_type {
                TokenType::Minus => Ok(Obj::NUMBER(left - right)),
                TokenType::Plus => Ok(Obj::NUMBER(left + right)),
                TokenType::Star => Ok(Obj::NUMBER(left * right)),
                TokenType::Slash => Ok(Obj::NUMBER(left / right)),
                _ => Err(RuntimeError::new(op.lexeme.clone(), format!("Invalid binary operator for numbers, given {}", op.lexeme), op.line)),
            }
        }
        _ => Err(RuntimeError::new(op.lexeme.clone(), format!("'{}' can only be applied to numbers, given: {}, {}", op.lexeme.clone(), paris.0, paris.1), op.line)),
    }
}

// Two cases:
// left and right are strings               =>combine the strings 
// left is a string and right is a int      => combine the string and int into a string
fn concatenate_values(pairs: (Obj, Obj), token: &Token) -> Result<Obj, RuntimeError> {
    match pairs {
        (Obj::STRING(mut v), Obj::STRING(v2)) => {
            v.push_str(&v2);
            Ok(Obj::STRING(v))
        },
        (Obj::STRING(mut v), Obj::NUMBER(v2)) => {
            v.push_str(&v2.to_string());
            Ok(Obj::STRING(v))
        }
        (Obj::NUMBER(v), Obj::STRING(v2)) => {
            let mut s = v.to_string();
            s.push_str(&v2);
            Ok(Obj::STRING(s))
        },
        _ => Err(RuntimeError::new(token.lexeme.clone(), format!("'{}' can only be applied to String and Numbers, given: {}, {}", token.lexeme, pairs.0, pairs.1), token.line)),
    }
}


fn determine_equality(pair: (Obj, Obj), token: &Token) -> Result<Obj, RuntimeError> {
    match token.token_type {
        TokenType::EqualEqual => {
             match pair {
                (Obj::BOOL(v), Obj::BOOL(v2)) => Ok(Obj::BOOL(v == v2)),
                (Obj::Nil, Obj::Nil) => Ok(Obj::BOOL(true)),
                (Obj::STRING(v), Obj::STRING(v2)) => Ok(Obj::BOOL(v == v2)),
                (Obj::NUMBER(v), Obj::NUMBER(v2)) => Ok(Obj::BOOL(v == v2)),
                _ => Ok(Obj::BOOL(false)),
            }
        },
        TokenType::BangEqual => {
            match pair {
                (Obj::BOOL(v), Obj::BOOL(v2)) =>Ok(Obj::BOOL(v != v2)),
                (Obj::Nil, Obj::Nil) => Ok(Obj::BOOL(false)),
                (Obj::STRING(v), Obj::STRING(v2)) => Ok(Obj::BOOL(v != v2)),
                (Obj::NUMBER(v), Obj::NUMBER(v2)) => Ok(Obj::BOOL(v != v2)),
                _ => Ok(Obj::BOOL(true)),
            }
        },
        _ => Err(RuntimeError::new(token.lexeme.clone(), "Invalid token type. Expected '==' or '!='.".to_string(), token.line)),
    }
}

fn determine_int_comparison(pair: (Obj, Obj), token: &Token) -> Result<Obj, RuntimeError> {
    match pair {
        (Obj::NUMBER(val), Obj::NUMBER(val2)) => {
            match token.token_type {
                TokenType::Less => Ok(Obj::BOOL(val < val2)),
                TokenType::LessEqual => Ok(Obj::BOOL(val <= val2)),
                TokenType::Greater => Ok(Obj::BOOL(val > val2)),
                TokenType::GreaterEqual => Ok(Obj::BOOL(val >= val2)),
                _ => panic!("Expected boolean values")
            } 
        }, 
        _ => Err(RuntimeError::new(token.lexeme.clone(), "Expected two numbers.".to_string(), token.line)),
    }
}

// Determines if  a value is truthy or falsy
// Important: Flax follows Ruby's rule: everything but False and nil are true
fn is_truthy(value: Obj) -> bool {
    match value {
        Obj::BOOL(false) | Obj::Nil => false,
        _ => true,
    }
}