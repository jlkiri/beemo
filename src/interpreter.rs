use crate::{
    env::Environment,
    error::*,
    function::Function,
    parser::Stmt,
    parser::{Expr, Value},
    scanner::TokenType,
};

#[derive(Debug)]
pub enum ErrorKind {
    WrongOperand,
    NotInfixOperator,
    ParameterArgumentMismatch,
    NothingReturned,
    VariableUndefined,
    ReturnNotExpression,
    NotCallable,
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
            self.eval_stmt(stmt, &self.globals)?
        }
        let main = self
            .globals
            .get("main")
            .ok_or(BeemoError::RuntimeError(ErrorKind::NoMain))?;
        match main {
            Value::Callable(f) => {
                f.call(self, Default::default())?;
            }
            _ => panic!("Main is not a function."),
        }
        Ok(())
    }

    pub fn eval_print_stmt(&self, val: Expr, env: &Environment) -> Result<()> {
        let expr = self.eval_expr(val, env)?;
        println!("PRINT STATEMENT: {:?}", expr);
        Ok(())
    }

    pub fn eval_cond_stmt(
        &self,
        cond: Expr,
        if_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
        env: &Environment,
    ) -> Result<()> {
        let condition = self.eval_expr(cond, env)?;
    }

    pub fn eval_function_decl(&self, fun: Function) -> Result<()> {
        self.globals.define(fun.name.clone(), Value::Callable(fun));
        Ok(())
    }

    pub fn eval_var_expr(&self, name: String, env: &Environment) -> Result<Value> {
        env.get(&name)
            .ok_or(BeemoError::RuntimeError(ErrorKind::VariableUndefined))
    }

    pub fn eval_call_expr(
        &self,
        callee: String,
        args: Vec<Expr>,
        env: &Environment,
    ) -> Result<Value> {
        let mut arguments = vec![];
        for expr in args {
            let value = self.eval_expr(expr, env)?;
            arguments.push(value);
        }
        let func = env
            .get(&callee)
            .ok_or(BeemoError::RuntimeError(ErrorKind::VariableUndefined))?;
        match func {
            Value::Callable(f) => {
                let ret = f.call(&self, arguments)?;
                Ok(ret)
            }
            _ => Err(BeemoError::RuntimeError(ErrorKind::NotCallable)),
        }
    }

    fn eval_binary_expr(
        &self,
        op: TokenType,
        lhs: Expr,
        rhs: Expr,
        env: &Environment,
    ) -> Result<Value> {
        let lexpr = self.eval_expr(lhs, env)?;
        let rexpr = self.eval_expr(rhs, env)?;
        match op {
            TokenType::Plus => match (lexpr, rexpr) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                _ => Err(BeemoError::RuntimeError(ErrorKind::WrongOperand)),
            },
            TokenType::Minus => match (lexpr, rexpr) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                _ => Err(BeemoError::RuntimeError(ErrorKind::WrongOperand)),
            },
            TokenType::Multiply => match (lexpr, rexpr) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                _ => Err(BeemoError::RuntimeError(ErrorKind::WrongOperand)),
            },
            TokenType::Divide => match (lexpr, rexpr) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
                _ => Err(BeemoError::RuntimeError(ErrorKind::WrongOperand)),
            },
            _ => Err(BeemoError::RuntimeError(ErrorKind::NotInfixOperator)),
        }
    }

    pub fn eval_expr(&self, expr: Expr, env: &Environment) -> Result<Value> {
        match expr {
            Expr::Variable(name) => self.eval_var_expr(name, env),
            Expr::Call(callee, args) => self.eval_call_expr(callee, args, env),
            Expr::Literal(value) => Ok(value),
            Expr::Binary(op, lhs, rhs) => self.eval_binary_expr(op, *lhs, *rhs, env),
        }
    }

    pub fn eval_block(&self, stmts: Vec<Stmt>, env: &Environment) -> Result<()> {
        let length = stmts.len();
        for (i, stmt) in stmts.into_iter().enumerate() {
            if i == length - 1 {
                match stmt {
                    Stmt::Expression(expr) => {
                        let result = self.eval_expr(expr, env)?;
                        env.define("return".to_string(), result);
                        return Ok(());
                    }
                    _ => {
                        self.eval_stmt(stmt, env)?;
                        env.define("return".to_string(), Value::Unit);
                        return Ok(());
                    }
                }
            }
            self.eval_stmt(stmt, env)?;
        }
        Ok(())
    }

    fn eval_stmt_finish(&self, stmt: Stmt, env: &Environment) -> Result<()> {
        match stmt {
            Stmt::FunctionDeclaration(fun) => self.eval_function_decl(fun),
            Stmt::Print(val) => self.eval_print_stmt(val, env),
            Stmt::Condition(cond, if_branch, else_branch) => {
                self.eval_cond_stmt(cond, if_branch, else_branch, env)
            }
            Stmt::Expression(expr) => {
                self.eval_expr(expr.clone(), env)?;
                Ok(())
            }
            _ => todo!(),
        }
    }

    pub fn eval_stmt(&self, stmt: Stmt, env: &Environment) -> Result<()> {
        self.eval_stmt_finish(stmt, env)
    }
}
