use std::collections::HashMap;
use crate::interpreter::Value;

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, Value>
}

impl Environment {
    pub fn new() -> Environment {
        Environment { values: HashMap::new() }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }
}