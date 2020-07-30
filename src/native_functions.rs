use std::fmt;
use crate::errors::RuntimeError;
use crate::callable::Callable;
use crate::interpreter::{Value, Interpreter};
use crate::environment::Environment;

#[derive(PartialEq, Clone)]
pub enum NativeFunctions {
    Clock,
    Println(Box<Println>),
    Print(Box<Print>),
}

#[derive(PartialEq, Clone)]
pub struct Println {
    value: Value,
}

#[derive(PartialEq, Clone)]
pub struct Print {
    value: Value,
}

impl NativeFunctions {
    pub fn new_print_func(value: Value) -> NativeFunctions {
        NativeFunctions::Print(Box::new(Print{ value }))
    }

    pub fn new_println_func(value: Value) -> NativeFunctions {
        NativeFunctions::Println(Box::new(Println{ value }))
    }
}

impl Callable for NativeFunctions {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>, env: &mut Environment, line: u64) -> Result<Value, RuntimeError> {
        match self {
            NativeFunctions::Clock => clock(),
            NativeFunctions::Println(expr) => expr.call(interpreter, args, env, line),
            NativeFunctions::Print(expr) => expr.call(interpreter, args, env, line), 
        }
    }
    fn arity(&self) -> u8 {
        match self {
            NativeFunctions::Clock => 0,
            NativeFunctions::Println(expr) => expr.arity(),
            NativeFunctions::Print(expr) => expr.arity(),
        }
    }
}

fn clock() -> Result<Value, RuntimeError> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let epoch_time = SystemTime::now().duration_since(UNIX_EPOCH).expect("[FATAL] failed to get system time").as_secs();
    Ok(Value::NUMBER(epoch_time as f64))
}

impl Callable for Print {
    fn call(&self, _interpreter: &mut Interpreter, args: Vec<Value>, _env: &mut Environment, _line: u64) -> Result<Value, RuntimeError> {
        if args.len() < 1 {
            print!("");
            return Ok(Value::Nil)
        }
        print!("{}", args[0]);
        Ok(Value::Nil)
    }

    fn arity(&self) -> u8 {
        1
    }
}

impl Callable for Println {
    fn call(&self, _interpreter: &mut Interpreter, args: Vec<Value>, _env: &mut Environment, _line: u64) -> Result<Value, RuntimeError> {
        if args.len() < 1 {
            println!("");
            return Ok(Value::Nil)
        }
        println!("{}", args[0]);
        Ok(Value::Nil)
    }

    fn arity(&self) -> u8 {
        1
    }
}



impl fmt::Debug for NativeFunctions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NativeFunctions::Clock => write!(f, "<fn clock>", ),
            NativeFunctions::Println(_value) => write!(f, "<fn println>"),
            NativeFunctions::Print(_value) => write!(f, "<fn print>"),
        }
    }
}