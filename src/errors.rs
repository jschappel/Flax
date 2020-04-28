use std::fmt;
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

#[derive(Debug)]
pub struct RuntimeError {
    line: u64,
    msg: String,
    operator: String, 
}





impl RuntimeError {
    fn new(op: String, msg: String, line: u64) -> RuntimeError {
        RuntimeError { operator: op, msg: msg, line: line }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<>) -> fmt::Result {
        write!(f, "[Runtime Error at line {}]: {}", self.line, self.msg)
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