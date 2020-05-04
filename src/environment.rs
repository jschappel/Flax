use std::collections::HashMap;
use crate::interpreter::Value;
use crate::errors::RuntimeError;
use crate::lexer::Token;

#[derive(Debug, Clone)]
pub enum EnvType {
    Global,
    Scoped(Box<Environment>)
}

#[derive(Debug, Clone)]
pub struct Environment {
    enclosing: EnvType,
    values: HashMap<String, Value>
}


impl Environment {
    pub fn new() -> Environment {
        Environment { values: HashMap::new(), enclosing: EnvType::Global }
    }

    pub fn new_lexical(&mut self) -> Environment {
        let outer = self.clone();
        Environment { values: HashMap::new(), enclosing:  EnvType::Scoped(Box::new(outer)) }
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
                    EnvType::Scoped(ref mut env) => env.get(token), 
                    EnvType::Global => Err(RuntimeError::new(token.lexeme.clone(), "Undefined Variable".to_string(), token.line))
                }
            }
        }
    }

    pub fn assign(&mut self, token: &Token, value: Value) -> Result<(), RuntimeError> {
        let name = token.lexeme.clone();
        if self.values.contains_key(&name) {
            self.values.insert(name, value);
            return Ok(());
        }
        if let EnvType::Scoped(ref mut x) = self.enclosing.clone() {
            return x.assign(token, value);
        }
        Err(RuntimeError::new(token.lexeme.clone(), "Undefined Variable".to_string(), token.line))
    }
}