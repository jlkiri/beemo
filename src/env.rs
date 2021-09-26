use std::{collections::HashMap, hash::Hash};

use crate::Value;

pub struct Environment {
    enclosing: Option<Box<Environment>>,
    inner: HashMap<String, Value>,
}

impl Environment {
    fn new() -> Self {
        Self {
            enclosing: None,
            inner: HashMap::new(),
        }
    }

    fn define(&mut self, ident: String, value: Value) {
        self.inner.insert(ident, value);
    }

    fn get(&self, ident: &str) -> Option<Value> {
        self.inner.get(ident).cloned()
    }
}
