use std::fmt;

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

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at line: {}", self.msg, self.line)
    }
}