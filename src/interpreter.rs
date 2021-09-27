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
    NoMain,
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

    pub fn interpret(&self, stmts: Vec<Stmt>) -> Result<()> {
        for stmt in stmts {
            self.eval_stmt(&stmt, &self.globals)?
        }
        let main = self
            .globals
            .get("main")
            .ok_or(BeemoError::RuntimeError(ErrorKind::NoMain))?;
        match main {
            Value::Callable(f) => {
                f.call(self, &vec![])?;
            }
            _ => panic!("main is not a function"),
        }
        Ok(())
    }

    pub fn eval_function_decl(&self, fun: &Function) -> Result<()> {
        self.globals
            .define(fun.name.clone(), Value::Callable(fun.clone()));
        Ok(())
    }

    pub fn eval_expr(&self, expr: Expr, env: &Environment) -> Result<Value> {
        match expr {
            Expr::Variable(name) => env
                .get(&name)
                .ok_or(BeemoError::RuntimeError(ErrorKind::VariableUndefined)),
            Expr::Call(callee, args) => {
                let arguments = args
                    .into_iter()
                    .map(|a| match a {
                        Expr::Literal(v) => v,
                        _ => panic!("whoops"),
                    })
                    .collect::<Vec<Value>>();
                let func = env
                    .get(&callee)
                    .ok_or(BeemoError::RuntimeError(ErrorKind::VariableUndefined))?;
                match func {
                    Value::Callable(f) => {
                        println!("CALLING A FUNC");
                        f.call(&self, &arguments);
                        Ok(Value::Unit)
                    }
                    _ => panic!("whoops not a callable"),
                }
            }
            _ => panic!("whoops not a call not a var"),
        }
    }

    pub fn eval_block(&self, stmts: &[Stmt], env: Environment) -> Result<()> {
        for stmt in stmts {
            self.eval_stmt(stmt, &env)?;
        }
        Ok(())
    }

    pub fn eval_stmt(&self, stmt: &Stmt, env: &Environment) -> Result<()> {
        match stmt {
            Stmt::FunctionDeclaration(fun) => self.eval_function_decl(fun),
            Stmt::Print(val) => {
                println!("PRINT STATEMENT: {:?}", self.eval_expr(val.clone(), env)?);
                Ok(())
            }
            Stmt::Expression(expr) => {
                self.eval_expr(expr.clone(), env)?;
                Ok(())
            }
            _ => todo!(),
        }
    }
}
