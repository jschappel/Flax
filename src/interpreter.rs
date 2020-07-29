use std::fmt;

use crate::ast::{Binary, Unary, Literal, Grouping, Expr, Stmt, Conditional, IfStatement, 
    Logical, Call, Function, Return};
use crate::callable::{FunctionTypes};
use crate::errors::{RuntimeError};
use crate::lexer::{TokenType, Token};
use crate::environment::{ Environment };
use crate::native_functions::NativeFunctions;
use crate::library::strlib::StrLib;
use crate::library::testlib::TestLib;

pub type RuntimeResult<T> = std::result::Result<T, RuntimeError>;

pub struct Interpreter {
    pub globals: Environment,
    pub environment: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let globals = Self::create_environment();
        let environment = globals.clone();
        Interpreter { globals, environment }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> RuntimeResult<()> {
        let mut globals = self.globals.clone();

        for statement in statements {
            statement.evaluate(self, &mut globals)?;
        }
        Ok(())
    }
/*
    pub fn execute_block(&mut self, body: &Stmt, env: &mut Environment) -> Result<Value, RuntimeError> {
        let value = body.evaluate(self, env)?;
        Ok(value)
    }
*/

    fn create_environment() -> Environment {
        let mut globals = Environment::new();
        globals.define(String::from("clock"), Some(Value::new_native_function(NativeFunctions::Clock)));
        globals.define(String::from("println"), Some(Value::new_native_function(NativeFunctions::new_println_func(Value::Nil))));
        globals.define(String::from("print"), Some(Value::new_native_function(NativeFunctions::new_print_func(Value::Nil))));
        globals.define(String::from("len"), Some(Value::new_str_function(StrLib::Len)));
        globals.define(String::from("charAt"), Some(Value::new_str_function(StrLib::CharAt)));
        globals.define(String::from("subString"), Some(Value::new_str_function(StrLib::SubStr)));
        globals.define(String::from("assert"), Some(Value::new_test_function(TestLib::Assert)));
        globals.define(String::from("assertEq"), Some(Value::new_test_function(TestLib::AssertEq)));
        globals
    }
}


pub trait Visit {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<Value>;
}


impl Visit for Stmt {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<Value> {
        match self {
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
                execute_block(&stmts, interpreter, env)?;
                return Ok(Value::Nil); // Dummy Value
            },
            Stmt::WhileStmt(ref cond, ref body) => {
                while is_truthy(&cond.evaluate(interpreter, env)?) {
                    if let Err(err) = body.evaluate(interpreter, env) {
                        if err == RuntimeError::Break {
                           break;
                        } else {
                           return Err(err);
                        }
                    }
                }
                return Ok(Value::Nil); // Dummy Value
            },
            Stmt::FuncStmt(func) => func.evaluate(interpreter, env),
            Stmt::ReturnStmt(stmt) => stmt.evaluate(interpreter, env),
            Stmt::Break => Err(RuntimeError::Break),
            Stmt::IfStmt(ref stmt) => stmt.evaluate(interpreter, env),
        }
    }
}

pub fn execute_block(statements: &Vec<Stmt>, interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<()> {
    let mut new_env = env.new_lexical();
    for statement in statements.iter() {
        statement.evaluate(interpreter, &mut new_env)?;
    }
    *env = new_env.return_outer_scope();
    Ok(())
}

impl Visit for Return {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<Value> {
        let value = match &self.expr {
            Some(expr) => Some(expr.evaluate(interpreter, env)?),
            None => None,
        };
        Err(RuntimeError::Return(value))
    }
}

impl Visit for IfStatement {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<Value> {
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

impl Visit for Function {
    fn evaluate(&self, _interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<Value> {
        let function = Value::Callable(FunctionTypes::new_function(self.clone(), env.clone()));
        env.define(self.name.lexeme.clone(), Some(function));
        Ok(Value::Nil)
    }
}


impl Visit for Expr {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<Value> {
        match self {
            Expr::L(ref inside_val)     => inside_val.evaluate(interpreter, env),
            Expr::B(ref inside_val)     => inside_val.evaluate(interpreter, env),
            Expr::U(ref inside_val)     => inside_val.evaluate(interpreter, env),
            Expr::G(ref inside_val)     => inside_val.evaluate(interpreter, env),
            Expr::C(ref inside_val)     => inside_val.evaluate(interpreter, env),
            Expr::Log(ref inside_val)   => inside_val.evaluate(interpreter, env),
            Expr::V(ref token)          => env.get(token),
            Expr::Cal(ref inside_val)   => inside_val.evaluate(interpreter, env),
            Expr::A(ref token, expr)    => {
                let value: Value = expr.evaluate(interpreter, env)?;
                env.assign(token, value.clone())?; //TODO: is this needed?
                // TODOFIX: value is not being set to 11 for some reason?????
                Ok(value)
            }
        }
    }
}

impl Visit for Literal {
    fn evaluate(&self, _interpreter: &mut Interpreter, _env: &mut Environment) -> RuntimeResult<Value> {
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
            return Err(RuntimeError::no_token_error("", String::from("Invalid literal value, given: {}"), 1)) //TODO: better error handling
        }
    }
}

impl Visit for Binary {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<Value> {
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
            _ => Err(RuntimeError::string_error(&self.operator, format!("Expected expression, given {}", self.operator.lexeme))),
        }
    }
}

impl Visit for Unary {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<Value> {
        let expr: Value = self.expr.evaluate(interpreter, env)?;

        match self.operator.token_type {
            TokenType::Minus => {
                if let Value::NUMBER(v) = expr {
                   return Ok(Value::NUMBER(-1.0 * v));
                }
                Err(RuntimeError::str_error(&self.operator,  "Invalid unary expression.  Expected Number"))
            },
            TokenType::Bang => Ok(Value::BOOL(!is_truthy(&expr))),
            _ => Err(RuntimeError::str_error(&self.operator, "Invalid token for Unary"))
        }
    }
}

impl Visit for Conditional {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<Value> {
        let cond: Value = self.cond.evaluate(interpreter, env)?;

        match cond {
            Value::BOOL(val) => {
                match val {
                    true => Ok(self.then_expr.evaluate(interpreter, env)?),
                    _ => Ok(self.else_expr.evaluate(interpreter, env)?),
                }
            },
            _ => Err(RuntimeError::no_token_error("?", format!("expected boolean given {}", cond), self.line_num)),
        }
    }
}

impl Visit for Logical {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<Value> {
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
            _ => Err(RuntimeError::str_error(&self.tok, "TODO: Better error handling")),
        }
    }
}

impl Visit for Call {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<Value> {
        let callee = self.callee.evaluate(interpreter, env)?;
        let arguments: Vec<Value> = self.args.iter().map(|arg| arg.evaluate(interpreter, env).unwrap()).collect::<Vec<Value>>();
        if let Value::Callable(callable) = callee {
            if callable.arity() != self.args.len() as u8 {
                return Err(RuntimeError::str_error(&self.tok, "Invalid callee"))
            }
           return Ok(callable.call(interpreter, arguments, env)?)       
        }
        Err(RuntimeError::no_token_error("", String::from("Can only call functions"), 10)) //TODO: Better error handling
    }
}



impl Visit for Grouping {
    fn evaluate(&self, interpreter: &mut Interpreter, env: &mut Environment) -> RuntimeResult<Value> {
        Ok(self.expr.evaluate(interpreter, env)?)
    }
}


#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    BOOL(bool),
    STRING(String),
    NUMBER(f64),
    Nil,
    Callable(FunctionTypes)
}

impl Value {
    pub fn new_native_function(func: NativeFunctions) -> Value {
        Value::Callable(FunctionTypes::new_native_func(func))
    }

    pub fn new_str_function(func: StrLib) -> Value {
        Value::Callable(FunctionTypes::str_lib_func(func))
    }

    pub fn new_test_function(func: TestLib) -> Value {
        Value::Callable(FunctionTypes::test_lib_func(func))
    }
    
}




fn check_numbers(paris: (Value, Value), op: &Token) -> RuntimeResult<Value> {
    match paris {
        (Value::NUMBER(left), Value::NUMBER(right)) => {
            match op.token_type {
                TokenType::Minus => Ok(Value::NUMBER(left - right)),
                TokenType::Plus => Ok(Value::NUMBER(left + right)),
                TokenType::Star => Ok(Value::NUMBER(left * right)),
                TokenType::Slash => {
                    if right == 0.0 {
                        return Err(RuntimeError::DivideByZero(op.line))
                    }
                    Ok(Value::NUMBER(left / right))
                },
                _ => Err(RuntimeError::string_error(op, format!("Invalid binary operator for numbers, given {}", op.lexeme))),
            }
        }
        _ => Err(RuntimeError::string_error(op, format!("'{}' can only be applied to numbers, given: {}, {}", op.lexeme.clone(), paris.0, paris.1))),
    }
}

// Two cases:
// left and right are strings               =>combine the strings 
// left is a string and right is a int      => combine the string and int into a string
fn concatenate_values(pairs: (Value, Value), token: &Token) -> RuntimeResult<Value> {
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
        _ => Err(RuntimeError::string_error(token, format!("'{}' can only be used for String Concatenation, given: {}, {}", token.lexeme, pairs.0, pairs.1))),
    }
}


fn determine_equality(pair: (Value, Value), token: &Token) -> RuntimeResult<Value> {
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
        _ => Err(RuntimeError::str_error(token, "Invalid token type. Expected '==' or '!='.")),
    }
}

fn determine_int_comparison(pair: (Value, Value), token: &Token) -> RuntimeResult<Value> {
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
        _ => Err(RuntimeError::str_error(token, "Expected two numbers")),
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
            Value::Callable(func) => write!(f, "{:?}", func),
        }
    }
}