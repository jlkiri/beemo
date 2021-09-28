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
    pub fn call(&self, interpreter: &Interpreter, arguments: &[Value]) -> Result<Value> {
        let env = interpreter.globals.child();
        for (i, arg) in arguments.iter().enumerate() {
            let param = self.params.get(i).ok_or(BeemoError::RuntimeError(
                interpreter::ErrorKind::ParameterArgumentMismatch,
            ))?;
            env.define(param.to_string(), arg.clone())
        }
        interpreter.eval_block(self.body.clone(), &env)?;
        env.get("return").ok_or(BeemoError::RuntimeError(
            interpreter::ErrorKind::NothingReturned,
        ))
    }
}
