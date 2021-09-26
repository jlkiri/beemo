use std::rc::Rc;

use crate::{
    env::Environment,
    interpreter::Interpreter,
    parser::{Expr, Value},
};

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<String>,
    pub name: String,
    pub body: Vec<Expr>,
}

impl Function {
    pub fn call(&self, interpreter: &Interpreter, arguments: &[Value]) {
        let env = Environment::with_parent(Rc::clone(&interpreter.globals));
    }
}
