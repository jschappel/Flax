use crate::callable::Callable;
use crate::interpreter::{Interpreter, Value, RuntimeResult};
use crate::environment::Environment;
use crate::errors::RuntimeError;

use std::fmt;

#[derive(PartialEq, Clone)]
pub enum TestLib {
    Assert,
    AssertEq,
}

impl Callable for TestLib {
    fn call(&self, _interpreter: &mut Interpreter, args: Vec<Value>, _env: &mut Environment) -> RuntimeResult<Value> {
        match self {
            TestLib::Assert => assert(&args[0]),
            TestLib::AssertEq => assert_eq(&args[0], &args[1]),
        }
    }

    fn arity(&self) -> u8 {
        match self {
            TestLib::Assert => 1,
            TestLib::AssertEq => 2,
        }
    }
}


fn assert(given: &Value) -> RuntimeResult<Value> {
    if is_truthy(given) {
        Ok(Value::BOOL(true))
    } else {
        Err(RuntimeError::AssertError(Value::BOOL(true), given.clone(), 1000))
    }
}

fn assert_eq(given: &Value, expected: &Value) -> RuntimeResult<Value> {
    if given != expected {
        Err(RuntimeError::AssertError(expected.clone(), given.clone(), 1000))
    } else {
        Ok(Value::Nil)
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

impl fmt::Debug for TestLib {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TestLib::Assert => write!(f, "<fn assert>",),
            TestLib::AssertEq => write!(f, "<fn assertEq>"),
        }
    }
}