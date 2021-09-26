use std::{collections::HashMap, rc::Rc};

use crate::{
    env::Environment,
    error::*,
    function::Function,
    parser::Stmt,
    parser::{Expr, Value},
};

#[derive(Debug)]
pub enum ErrorKind {
    Placeholder,
    VariableUndefined,
}

pub struct Interpreter {
    pub globals: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            globals: Environment::new(),
        }
    }

    pub fn eval_function_decl(&mut self, fun: &Function) -> Result<()> {
        self.globals
            .define(fun.name.clone(), Value::Callable(fun.clone()));
        Ok(())
    }

    pub fn eval_expr(&mut self, expr: &Expr, env: Environment) -> Result<Value> {
        match expr {
            Expr::Variable(name) => env
                .get(&name)
                .ok_or(BeemoError::RuntimeError(ErrorKind::VariableUndefined)),
            _ => todo!(),
        }
    }

    pub fn eval_block(&mut self, exprs: &Expr, env: Environment) -> Result<Value> {
        self.eval_expr(exprs, env)
    }

    pub fn eval_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::FunctionDeclaration(fun) => self.eval_function_decl(fun),
            _ => todo!(),
        }
    }
}
