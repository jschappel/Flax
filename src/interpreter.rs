use crate::ast::{Binary, Unary, Literal, Grouping, Expr, Stmt, Conditional, IfStatement, Logical};
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
        let mut env = Environment::new();
        for statement in statements {
            statement.evaluate(self, &mut env)?;
        }
        Ok(())
    }

    // fn add_global_var(&mut self, name: String, value: Value) {
    //     self.environment.define(name, Some(value))
    // }

    // fn get_global_var(&mut self, token: &Token) -> Result<Value, RuntimeError> {
    //     self.environment.get(token)
    // }

    // fn update_global(&mut self, token: &Token, value: Value) -> Result<(), RuntimeError> {
    //     self.environment.assign(token, value)
    // }
}


pub trait Visit {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value, RuntimeError>;
}


impl Visit for Stmt {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value, RuntimeError> {
        match self {
            Stmt::PrintStmt(expr) => {
                let value: Value = expr.evaluate(interpreter, env)?;
                println!("{}", value);
                Ok(value)
            },
            Stmt::ExprStmt(expr) => expr.evaluate(interpreter, env),
            Stmt::VarDecl(token, opt) => {
                match opt {
                    Some(expr) => {
                        let value = expr.evaluate(interpreter, env)?;
                        env.define(token.lexeme.clone(), Some(value));
                    },
                    None => env.define(token.lexeme.clone(), None),
                }
                return Ok(Value::Nil); // Dummy Value
            },
            Stmt::Block(ref stmts) => {
                let mut new_env = env.new_lexical();
                for statement in stmts.iter() {
                    if statement.evaluate(interpreter, &mut new_env)? == Value::Break {
                       return Ok(Value::Break);
                    }
                }
                // TODO:: Better memory management
                *env = new_env.return_outer_scope();
                return Ok(Value::Nil); // Dummy Value
            },
            Stmt::WhileStmt(ref cond, ref body) => {
                while is_truthy(&cond.evaluate(interpreter, env)?) {
                   if body.evaluate(interpreter, env)? == Value::Break {
                       break;
                   }
                }
                return Ok(Value::Nil); // Dummy Value
            },
            Stmt::Break => Ok(Value::Break),
            Stmt::IfStmt(ref stmt) => stmt.evaluate(interpreter, env),
        }
    }
}

impl Visit for IfStatement {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value, RuntimeError> {
        let cond = self.conditional.evaluate(interpreter, env)?;
        match cond {
            Value::BOOL(v) => {
                if v == false {
                    return match &self.else_block {
                        Some(block) => Ok(block.evaluate(interpreter, env)?),
                        None => Ok(Value::Nil) // Dummy value
                    }
                }
                return Ok(self.then_block.evaluate(interpreter, env)?);
            },
            _ => self.then_block.evaluate(interpreter, env),
        }
    }
}


impl Visit for Expr {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value, RuntimeError> {
        match self {
            Expr::L(ref inside_val)     => inside_val.evaluate(interpreter, env),
            Expr::B(ref inside_val)     => inside_val.evaluate(interpreter, env),
            Expr::U(ref inside_val)     => inside_val.evaluate(interpreter, env),
            Expr::G(ref inside_val)     => inside_val.evaluate(interpreter, env),
            Expr::C(ref inside_val)     => inside_val.evaluate(interpreter, env),
            Expr::Log(ref inside_val)   => inside_val.evaluate(interpreter, env),
            Expr::V(ref token)          => env.get(token),
            Expr::A(ref token, expr)    => {
                let value: Value = expr.evaluate(interpreter, env)?;
                env.assign(token, value.clone())?;
                Ok(value)
            }
        }
    }
}

impl Visit for Literal {
    fn evaluate(&self, _interpreter: &mut Interpreter, _env: &mut Environment) -> Result<Value, RuntimeError> {
        if self.val.parse::<f64>().is_ok() {
            Ok(Value::NUMBER(self.val.parse::<f64>().unwrap()))
        }
        else if self.val.parse::<bool>().is_ok() && self.val != "true" && self.val != "false" {
            Ok(Value::BOOL(self.val.parse::<bool>().unwrap()))
        }
        else if self.val.parse::<String>().is_ok() {
            let s = self.val.parse::<String>().unwrap();
            match &s[..] {
                "nil" => Ok(Value::Nil),
                "true" => Ok(Value::BOOL(true)),
                "false" => Ok(Value::BOOL(false)),
                _ => Ok(Value::STRING(s)),
            }
        }
        else {
            Err(RuntimeError::new(String::new(), format!("Invalid literal value, given: {}", self.val), 1)) //TODO:Better Error handling
        }
    }
}

impl Visit for Binary {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value, RuntimeError> {
        let right: Value = self.right.evaluate(interpreter, env)?;
        let left: Value = self.left.evaluate(interpreter, env)?;
        
        match self.operator.token_type {
            TokenType::Minus => check_numbers((left, right), &self.operator),
            TokenType::Plus => check_numbers((left, right), &self.operator),
            TokenType::Star => check_numbers((left, right), &self.operator),
            TokenType::Slash => check_numbers((left, right), &self.operator),
            TokenType::PlusPlus => concatenate_values((left, right), &self.operator),
            TokenType::EqualEqual => determine_equality((left, right), &self.operator),
            TokenType::BangEqual => determine_equality((left, right), &self.operator),
            TokenType::Less => determine_int_comparison((left, right), &self.operator),
            TokenType::LessEqual => determine_int_comparison((left, right), &self.operator),
            TokenType::Greater => determine_int_comparison((left, right), &self.operator),
            TokenType::GreaterEqual => determine_int_comparison((left, right), &self.operator),
            _ => Err(RuntimeError::new(self.operator.lexeme.clone(), format!("Expected expression, given {}", self.operator.lexeme), self.operator.line)),
        }
    }
}

impl Visit for Unary {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value, RuntimeError> {
        let expr: Value = self.expr.evaluate(interpreter, env)?;

        match self.operator.token_type {
            TokenType::Minus => {
                if let Value::NUMBER(v) = expr {
                   return Ok(Value::NUMBER(-1.0 * v));
                }
                Err(RuntimeError::new(self.operator.lexeme.clone(), "Invalid unary expression.  Expected Number".to_string(), self.operator.line))
            },
            TokenType::Bang => Ok(Value::BOOL(!is_truthy(&expr))),
            _ => Err(RuntimeError::new(self.operator.lexeme.clone(), "Invalid token for Unary".to_string(), self.operator.line)),
        }
    }
}

impl Visit for Conditional {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value, RuntimeError> {
        let cond: Value = self.cond.evaluate(interpreter, env)?;

        match cond {
            Value::BOOL(val) => {
                match val {
                    true => Ok(self.then_expr.evaluate(interpreter, env)?),
                    _ => Ok(self.else_expr.evaluate(interpreter, env)?),
                }
            },
            _ => Err(RuntimeError::new("?".to_string(), format!("expected boolean given {}", cond), self.line_num)),
        }
    }
}

impl Visit for Logical {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value, RuntimeError> {
        let left: Value = self.left.evaluate(interpreter, env)?;
        match self.tok.token_type {
            TokenType::Or => {
                if is_truthy(&left) {
                   return Ok(left)
                }
                else if !is_truthy(&left) {
                    return Ok(left)
                } else {
                    return Ok(self.right.evaluate(interpreter, env)?);
                }
            },
            TokenType::And => {
                if is_truthy(&left) {
                    return Ok(self.right.evaluate(interpreter, env)?);
                } else {
                    return Ok(left);
                }
            },
            _ => Err(RuntimeError::new(self.tok.lexeme.clone(), "TODO: Better error handling".to_string(), self.tok.line)),

        }
    }
}



impl Visit for Grouping {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value, RuntimeError> {
        Ok(self.expr.evaluate(interpreter, env)?)
    }
}


#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    BOOL(bool),
    STRING(String),
    NUMBER(f64),
    Nil,
    Break,
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
fn is_truthy(value: &Value) -> bool {
    match value {
        Value::BOOL(false) | Value::Nil => false,
        _ => true,
    }
}


 // *** DISPLAY trait implementations below ***

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        match self {
            Value::BOOL(val) => write!(f, "{}", val),
            Value::Nil => write!(f, "nil"),
            Value::STRING(val) => write!(f, "\"{}\"", val),
            Value::NUMBER(val) => write!(f, "{}", val),
            Value::Break => write!(f, "break"),
        }
    }
}


