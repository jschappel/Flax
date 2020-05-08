use std::fmt;

use crate::ast;
use crate::interpreter;
use crate::environment;
use crate::errors::RuntimeError;
use crate::native_functions::NativeFunctions;

use environment::Environment;
use interpreter::{Interpreter, Value};
use ast::Function as AstFunc;



pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>, env: &mut Environment) -> Result<Value, RuntimeError>;
    fn arity(&self) -> u8;
}

#[derive(PartialEq, Clone)]
pub enum FunctionTypes {
    Function(FlaxFunction),
    NativeFunction(NativeFunctions),
}

impl FunctionTypes {
    pub fn new_function(declaration: AstFunc) -> FunctionTypes {
        FunctionTypes::Function(FlaxFunction::new(declaration))
    }

    pub fn new_native_func(func: NativeFunctions) -> FunctionTypes {
        FunctionTypes::NativeFunction(func)
    }
}

impl FunctionTypes {
    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>, env: &mut Environment) -> Result<Value, RuntimeError> {
        match self {
            FunctionTypes::Function(func) => func.call(interpreter, args, env),
            FunctionTypes::NativeFunction(func) => func.call(interpreter, args, env),
        }
    }

    pub fn arity(&self) -> u8 {
        match self {
            FunctionTypes::Function(func) => func.arity(),
            FunctionTypes::NativeFunction(func) => func.arity(),
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
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>, env: &mut Environment) -> Result<Value, RuntimeError> {
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




impl fmt::Debug for FunctionTypes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionTypes::Function(func) => write!(f, "<fn {}>", func.declaration.name),
            FunctionTypes::NativeFunction(func) => write!(f, "{:?}", func),
        }
    }
}