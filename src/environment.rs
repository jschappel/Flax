use std::collections::HashMap;
use crate::interpreter::Value;
use crate::errors::RuntimeError;
use crate::lexer::Token;

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, Value>
}


impl Environment {
    pub fn new() -> Environment {
        Environment { values: HashMap::new() }
    }

    pub fn define(&mut self, name: String, value: Option<Value>) {
        match value {
            Some(val) => {self.values.insert(name, val.clone());},
            None => {self.values.insert(name, Value::Nil);}
        }
    }

    pub fn get(&mut self, token: &Token) -> Result<Value, RuntimeError> {
        match self.values.get(&token.lexeme) {
            Some(val) => Ok(val.clone()),
            None => Err(RuntimeError::new(token.lexeme.clone(), "Undefined Variable".to_string(), token.line))
        }
    }
}