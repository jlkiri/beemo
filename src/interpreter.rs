use std::{collections::HashMap, rc::Rc};

use crate::{env::Environment, error::*, function::Function, parser::Stmt, parser::Value};

#[derive(Debug)]
pub enum ErrorKind {
    Placeholder,
}

pub struct Interpreter {
    pub globals: Rc<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            globals: Rc::new(Environment::new()),
        }
    }

    pub fn eval_function_decl(&mut self, fun: &Function) -> Result<()> {
        self.globals
            .define(fun.name.clone(), Value::Callable(fun.clone()));
        Ok(())
    }

    pub fn eval_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::FunctionDeclaration(fun) => self.eval_function_decl(fun),
            _ => todo!(),
        }
    }
}
