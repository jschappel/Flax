use std::fmt;

use crate::ast;
use crate::interpreter;
use crate::environment;
use crate::errors::RuntimeError;
use crate::native_functions::NativeFunctions;
use crate::library::strlib::StrLib;
use crate::library::testlib::TestLib;

use environment::Environment;
use interpreter::{Interpreter, Value, execute_block};
use ast::Function as AstFunc;



pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>, env: &mut Environment, line: u64) -> Result<Value, RuntimeError>;
    fn arity(&self) -> u8;
}

#[derive(PartialEq, Clone)]
pub enum FunctionTypes {
    Function(FlaxFunction),
    NativeFunction(NativeFunctions),
    TestLibrary(TestLib),
    StringLibrary(Box<StrLib>),
}

impl FunctionTypes {
    pub fn new_function(declaration: AstFunc, closure: Environment) -> FunctionTypes {
        FunctionTypes::Function(FlaxFunction::new(declaration, closure))
    }

    pub fn new_native_func(func: NativeFunctions) -> FunctionTypes {
        FunctionTypes::NativeFunction(func)
    }

    pub fn str_lib_func(func: StrLib) -> FunctionTypes {
        FunctionTypes::StringLibrary(Box::new(func))
    }

    pub fn test_lib_func(func: TestLib) -> FunctionTypes {
        FunctionTypes::TestLibrary(func)
    }
}

impl FunctionTypes {
    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>, env: &mut Environment, line: u64) -> Result<Value, RuntimeError> {
        match self {
            FunctionTypes::Function(func) => func.call(interpreter, args, env, line),
            FunctionTypes::NativeFunction(func) => func.call(interpreter, args, env, line),
            FunctionTypes::StringLibrary(func) => func.call(interpreter, args, env, line),
            FunctionTypes::TestLibrary(func) => func.call(interpreter, args, env, line),
        }
    }

    pub fn arity(&self) -> u8 {
        match self {
            FunctionTypes::Function(func) => func.arity(),
            FunctionTypes::NativeFunction(func) => func.arity(),
            FunctionTypes::StringLibrary(func) => func.arity(),
            FunctionTypes::TestLibrary(func) => func.arity(),
        }
    }
}




#[derive(PartialEq, Debug, Clone)]
pub struct FlaxFunction {
    declaration: AstFunc,
    closure: Environment,
}

impl FlaxFunction {
    pub fn new(declaration: AstFunc, closure: Environment) -> FlaxFunction {
        FlaxFunction { declaration, closure }
    }
}


impl Callable for FlaxFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>, _env: &mut Environment, line: u64) -> Result<Value, RuntimeError> {
        let mut env = self.closure.clone().new_lexical();
        let func_name = &self.declaration.name.lexeme;
        for (i, token) in self.declaration.params.iter().enumerate() {
            env.define(token.lexeme.clone(), Some(args[i].clone()))
        }
        env.define(func_name.clone(), Some(Value::Callable(FunctionTypes::Function(self.clone()))));
        
        let value = execute_block(&self.declaration.body, interpreter, &mut env);
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
            FunctionTypes::StringLibrary(func) => write!(f, "{:?}", func),
            FunctionTypes::TestLibrary(func) => write!(f, "{:?}", func),
        }
    }
}