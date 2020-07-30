use std::fmt;
use crate::errors::RuntimeError;
use crate::callable::Callable;
use crate::interpreter::{Value, Interpreter};
use crate::environment::Environment;


#[derive(PartialEq, Clone)]
pub enum StrLib {
    Len,
    CharAt,
    SubStr,
}


impl Callable for StrLib {
    fn call(&self, _interpreter: &mut Interpreter, args: Vec<Value>, _env: &mut Environment, line: u64) -> Result<Value, RuntimeError> { 
        match self {
            StrLib::Len => len(&args[0], line),
            StrLib::CharAt => char_at((&args[0], &args[1]), line),
            StrLib::SubStr => sub_str((&args[0], &args[1], &args[2]), line),
        }
    }

    fn arity(&self) -> u8 {
        match self {
            StrLib::Len => 1,
            StrLib::CharAt => 2,
            StrLib::SubStr => 3,
        }
    }
}


fn len(val: &Value, line: u64) -> Result<Value, RuntimeError> {
    if let Value::STRING(word) = val {
        return Ok(Value::NUMBER(word.len() as f64))
    }
    Err(RuntimeError::no_token_error("len", "Expected String".to_string(), line))
}

fn char_at(args: (&Value, &Value), line: u64) -> Result<Value, RuntimeError> {
    match args {
        (Value::NUMBER(i), Value::STRING(s)) => {
            let c = s.chars().nth(*i as usize).unwrap();
            Ok(Value::STRING(c.to_string()))
        },
        _ => Err(RuntimeError::no_token_error("len", "charAt expects Number, String".to_string(), line))
    }
}

fn sub_str(args: (&Value, &Value, &Value), line: u64) -> Result<Value, RuntimeError> {
    match args {
        (Value::NUMBER(start), Value::NUMBER(end),  Value::STRING(s)) => {
            let slice = &s[*start as usize..*end as usize];
            Ok(Value::STRING(String::from(slice)))
        }
        _ => Err(RuntimeError::no_token_error("len", "charAt expects Number, String".to_string(), line))
    }

}


impl fmt::Debug for StrLib {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StrLib::Len => write!(f, "<fn len>", ),
            StrLib::CharAt => write!(f, "<fn charAt>"),
            StrLib::SubStr => write!(f, "<fn subStr>"),
        }
    }
}