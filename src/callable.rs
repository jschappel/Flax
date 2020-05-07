use crate::ast;
use crate::interpreter;
use crate::environment;
use crate::errors::RuntimeError;
use environment::Environment;
use interpreter::{Interpreter, Value};
use ast::Function as AstFunc;

use std::fmt;

trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>, env: Environment) -> Result<Value, RuntimeError>;
    fn arity(&self) -> u8;
}

#[derive(PartialEq, Debug, Clone)]
pub enum FunctionTypes {
    Clock,
    Function(FlaxFunction),
}

impl FunctionTypes {
    pub fn new_function(declaration: AstFunc) -> FunctionTypes {
        FunctionTypes::Function(FlaxFunction::new(declaration))
    }
}

impl FunctionTypes {
    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>, env: Environment) -> Result<Value, RuntimeError> {
        match self {
            FunctionTypes::Function(func) => func.call(interpreter, args, env),
            Clock => clock(),
        }
    }

    pub fn arity(&self) -> u8 {
        match self {
            FunctionTypes::Function(func) => func.arity(),
            FunctionTypes::Clock => 0,
        }
    }
}




#[derive(PartialEq, Debug, Clone)]
pub struct FlaxFunction {
    declaration: AstFunc,
}

impl FlaxFunction {
    pub fn new(declaration: AstFunc) -> FlaxFunction {
        FlaxFunction { declaration }
    }
}


impl Callable for FlaxFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>, env: Environment) -> Result<Value, RuntimeError> {
        let mut env = env.clone().new_lexical();
        for (i, token) in self.declaration.params.iter().enumerate() {
            env.define(token.lexeme.clone(), Some(args[i].clone()))
        }
        let value = interpreter.interpret_function(&self.declaration.body, &mut env);
        match value {
            Ok(_value) => Ok(Value::Nil),
            Err(e) => {
                return match e {
                    RuntimeError::Return(Some(val)) => Ok(val),
                    RuntimeError::Return(None) => Ok(Value::Nil),
                    _ => Err(e),
                }
            }
        }
    }

    fn arity(&self) -> u8 {
        self.declaration.params.len() as u8
    }
}


fn clock() -> Result<Value, RuntimeError> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let epoch_time = SystemTime::now().duration_since(UNIX_EPOCH).expect("[FATAL] failed to get system time").as_secs();
    Ok(Value::NUMBER(epoch_time as f64))
}

impl fmt::Display for FunctionTypes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionTypes::Function(func) => write!(f, "{}", func),
            FunctionTypes::Clock => write!(f, "Here"),
        }
    }
}
impl fmt::Display for FlaxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.declaration.name)
    }
}