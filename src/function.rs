use crate::error::*;
use crate::interpreter;
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
    pub fn call(&self, interpreter: &mut Interpreter, arguments: &[Value]) -> Result<Value> {
        let mut env = interpreter.globals.child();
        for (i, arg) in arguments.iter().enumerate() {
            let param = self.params.get(i).ok_or(BeemoError::RuntimeError(
                interpreter::ErrorKind::VariableUndefined,
            ))?;
            env.define(param.to_string(), arg.clone())
        }
        let result = interpreter
            .eval_block(self.body.iter().next().unwrap(), env)
            .expect("Failed to execute function");
        Ok(result)
    }
}
