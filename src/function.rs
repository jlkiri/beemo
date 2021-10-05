use crate::error::*;
use crate::interpreter;
use crate::parser::Stmt;

use crate::{interpreter::Interpreter, parser::Value};

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<String>,
    pub name: String,
    pub body: Vec<Stmt>,
}

impl Function {
    pub fn call(&self, interpreter: &Interpreter, arguments: Vec<Value>) -> Result<Value> {
        let mut env = interpreter.globals.child();
        for (i, arg) in arguments.into_iter().enumerate() {
            let param = self.params.get(i).ok_or(BeemoError::RuntimeError(
                interpreter::ErrorKind::ParameterArgumentMismatch,
            ))?;
            env.define(param.to_string(), arg)
        }
        interpreter.eval_block(self.body.clone(), &mut env)?;
        match env.get("return") {
            Some(ret) => Ok(ret),
            _ => env.get("unit_return").ok_or(BeemoError::RuntimeError(
                interpreter::ErrorKind::NothingReturned,
            )),
        }
    }
}
