use std::collections::HashMap;
use crate::interpreter::Value;
use crate::errors::RuntimeError;
use crate::lexer::Token;


#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    enclosing: Option<Box<Environment>>,
    values: HashMap<String, Value>
}

impl Environment {
    pub fn new() -> Environment {
        Environment { values: HashMap::new(), enclosing: None }
    }

    pub fn new_lexical(&mut self) -> Environment {
        let outer = self.clone();
        Environment { values: HashMap::new(), enclosing:  Some(Box::new(outer)) }
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
            None => {
                match self.enclosing {
                    Some(ref mut env) => env.get(token), 
                    None => Err(RuntimeError::string_error(&token, format!("Undefined Identifier: {}", token.lexeme)))
                }
            }
        }
    }

    pub fn assign(&mut self, token: &Token, value: Value) -> Result<(), RuntimeError> {
        let name = token.lexeme.clone();
        if self.values.contains_key(&name) {
            self.values.insert(name, value);
            return Ok(())
        }
        if let Some(ref mut x) = self.enclosing {
            return x.assign(token, value);
        }
        Err(RuntimeError::string_error(&token, format!("Undefined identifier: {}", token.lexeme)))
    }

    // TODO:: Better memory management
    pub fn return_outer_scope(self) -> Environment {
        match self.enclosing {
            None => self,
            Some(ref env) => *env.clone(),
        }
    }
}