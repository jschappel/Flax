use std::fmt;
use std::convert::From;
use crate::lexer::Token;
use crate::interpreter::Value;

// A Lex Error is an error that the Lexer can throw 
#[derive(Debug)]
pub struct LexError {
    line: u64,
    msg: String,
}
impl LexError {
    pub fn new(line: u64, msg: String) -> LexError {
        LexError { line, msg }
    }
}


// A Parse Error is an error that the Parser can throw 
#[derive(Debug)]
pub struct ParseError {
    line: u64,
    msg: String
}
impl ParseError {
    pub fn new(msg:String, line: u64,) -> ParseError {
        ParseError { line, msg }
    }
}


#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    RuntimeError(String, u64, String),
    AssertError(Value, Value, u64),
    DivideByZero(u64),
    Return(Option<Value>),
    Break,
}


/// This is only used for the repel
pub enum ComplierError {
    LexError(LexError),
    ParseError(ParseError),
    RuntimeError(RuntimeError),
}

impl From<LexError> for ComplierError {
    fn from(error: LexError) -> Self {
        ComplierError::LexError(error)
    }
}

impl From<ParseError> for ComplierError {
    fn from(error: ParseError) -> Self {
        ComplierError::ParseError(error)
    }
}

impl From<RuntimeError> for ComplierError {
    fn from(error: RuntimeError) -> Self {
        ComplierError::RuntimeError(error)
    }
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


impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "[LexerError line {}]: {}", self.line, self.msg)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "[ParseError line {}]: {}", self.line, self.msg)
    }
}

impl fmt::Display for ComplierError {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        match self {
            ComplierError::LexError(e)     =>  write!(f, "{}", e),
            ComplierError::ParseError(e)   =>  write!(f, "{}", e),
            ComplierError::RuntimeError(e) =>  write!(f, "{}", e),
        }
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
            },
            RuntimeError::AssertError(expected, actual, line) => {
                write!(f, "[AssertError line {}]: Expected {}, given {}", line, expected, actual)
            },
            _ => write!(f, "RuntimeError: TODO: FINISH"), //TODO: Finish here????
        }
    }
}