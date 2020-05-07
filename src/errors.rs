use std::fmt;
use std::error::Error;
use crate::lexer::Token;
use crate::interpreter::Value;

// A Lex Error is an error that the Lexer can throw 
#[derive(Debug)]
pub struct LexError {
    line: u64,
    msg: String,
}

// A Parse Error is an error that the Parser can throw 
#[derive(Debug)]
pub struct ParseError {
    line: u64,
    msg: String
}


#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    RuntimeError(String, u64, String),
    DivideByZero(u64),
    Return(Option<Value>),
    Break,
}

impl RuntimeError {
    pub fn string_error(token: &Token, msg: String) -> RuntimeError {
        let line = token.line;
        let op = token.lexeme.clone();
        RuntimeError::RuntimeError(op, line, msg)
    }
    
    pub fn str_error(token: &Token, msg: &str) -> RuntimeError {
        let line = token.line;
        let op = token.lexeme.clone();
        RuntimeError::RuntimeError(op, line, msg.to_string())
    }

    pub fn no_token_error(operator: &str, msg: String,  line: u64) -> RuntimeError {
        RuntimeError::RuntimeError(operator.to_string(), line, msg)
    }
}


impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        match self {
            RuntimeError::RuntimeError(_op, line, msg) => {
                write!(f, "[RuntimeError line {}]: {}", line, msg)
            },
            RuntimeError::DivideByZero(line) => {
                write!(f, "[RuntimeError line {}]: Cannot Divide by 0", line)
            }
            _ => write!(f, "RuntimeError"),
        }
    }
}

impl Error for RuntimeError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}



impl LexError {
    pub fn new(line: u64, msg: String) -> LexError {
        LexError { line, msg }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at line: {}", self.msg, self.line)
    }
}



impl ParseError {
    pub fn new(msg:String, line: u64,) -> ParseError {
        ParseError { line, msg }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at line: {}", self.msg, self.line)
    }
}