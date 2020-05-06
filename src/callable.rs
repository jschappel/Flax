use crate::ast;
use crate::interpreter;
use crate::environment;
use crate::errors::RuntimeError;
use environment::Environment;
use interpreter::{Interpreter, Value};
use ast::Function as AstFunc;

pub enum FunctionTypes {
    Clock,
    Function(FlaxFunction),
}


pub struct FlaxFunction {
    declaration: AstFunc,
}

impl FlaxFunction {
    pub fn new(declaration: AstFunc) -> FlaxFunction {
        FlaxFunction { declaration }
    }
}

impl Callable for FlaxFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let mut env = interpreter.globals.new_lexical();
        for (i, token) in self.declaration.params.iter().enumerate() {
            env.define(token.lexeme, Some(args[i]))
        }
        let value = interpreter.interpret_function(self.declaration.body, &mut env)?;
        Ok(value)
    }


    fn arity(&self) -> u8 {

    }
}



trait Callable {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError>;
    fn arity(&self) -> u8;
}