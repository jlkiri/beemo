use core::panic;
use std::time::Instant;

use crate::{
    env::Environment,
    error::*,
    function::*,
    parser::Stmt,
    parser::{Expr, Value},
    scanner::TokenType,
};

#[derive(Debug)]
pub enum ErrorKind {
    WrongOperand,
    NotInfixOperator,
    NotUnaryOperator,
    NotBoolean,
    NotNumber,
    ParameterArgumentMismatch,
    NothingReturned,
    NotComparable,
    VariableUndefined,
    ReturnNotExpression,
    NotCallable,
    NotIndexable,
    CannotBeIndexedBy,
    NoMain,
}

pub struct Interpreter<'a> {
    pub globals: Environment<'a>,
}

#[derive(Debug, Clone)]
struct TimeStart;
impl Callable for TimeStart {
    fn call(&self, interpreter: &Interpreter, arguments: Vec<Value>) -> Result<Value> {
        interpreter
            .globals
            .define("__timestart".to_string(), Value::Instant(Instant::now()));
        Ok(Value::Unit)
    }
}

#[derive(Debug, Clone)]
struct TimeEnd;
impl Callable for TimeEnd {
    fn call(&self, interpreter: &Interpreter, _arguments: Vec<Value>) -> Result<Value> {
        let start = interpreter
            .globals
            .get("__timestart")
            .expect("Didn't start a timer.");
        match start {
            Value::Instant(inst) => {
                println!("{:?}ms", (Instant::now() - inst).as_millis());
            }
            _ => unreachable!(),
        }
        Ok(Value::Unit)
    }
}

impl Interpreter<'_> {
    pub fn new() -> Self {
        let globals = Environment::new();
        globals.define(
            "time_start".to_string(),
            Value::Callable(Box::new(TimeStart)),
        );
        globals.define("time_end".to_string(), Value::Callable(Box::new(TimeEnd)));
        Self { globals }
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
        println!("{:?}", expr);
        Ok(())
    }

    pub fn eval_cond_stmt(
        &self,
        cond: Expr,
        if_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
        env: &Environment,
    ) -> Result<()> {
        let condition = self.eval_expr(cond, &env)?;
        if !condition.is_bool() {
            return Err(BeemoError::RuntimeError(ErrorKind::NotBoolean));
        }
        match condition {
            Value::Bool(true) => self.eval_block(if_branch, &env)?,
            Value::Bool(false) => {
                if else_branch.is_some() {
                    self.eval_block(else_branch.unwrap(), &env)?;
                }
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    pub fn eval_function_decl(&self, fun: Function) -> Result<()> {
        self.globals
            .define(fun.name.clone(), Value::Callable(Box::new(fun)));
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
                let ret = f.call(self, arguments)?;
                Ok(ret)
            }
            _ => Err(BeemoError::RuntimeError(ErrorKind::NotCallable)),
        }
    }

    fn eval_logical_expr(
        &self,
        op: TokenType,
        lhs: Expr,
        rhs: Expr,
        env: &Environment,
    ) -> Result<Value> {
        let lexpr = self.eval_expr(lhs, env)?;
        match op {
            TokenType::Keyword(kw) if kw == "or" => {
                if matches!(lexpr, Value::Bool(true)) {
                    Ok(lexpr)
                } else {
                    let rexpr = self.eval_expr(rhs, env)?;
                    match (lexpr, rexpr) {
                        (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l || r)),
                        _ => Err(BeemoError::RuntimeError(ErrorKind::WrongOperand)),
                    }
                }
            }
            TokenType::Keyword(kw) if kw == "and" => {
                if matches!(lexpr, Value::Bool(false)) {
                    Ok(lexpr)
                } else {
                    let rexpr = self.eval_expr(rhs, env)?;
                    match (lexpr, rexpr) {
                        (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l && r)),
                        _ => Err(BeemoError::RuntimeError(ErrorKind::WrongOperand)),
                    }
                }
            }
            _ => Err(BeemoError::RuntimeError(ErrorKind::NotInfixOperator)),
        }
    }

    fn eval_unary_expr(&self, op: TokenType, expr: Expr, env: &Environment) -> Result<Value> {
        match op {
            TokenType::Bang => {
                let bool = self.eval_expr(expr, env)?;
                match bool {
                    Value::Bool(bool) => Ok(Value::Bool(!bool)),
                    _ => Err(BeemoError::RuntimeError(ErrorKind::NotBoolean)),
                }
            }
            TokenType::Minus => {
                let val = self.eval_expr(expr, env)?;
                match val {
                    Value::Number(num) => Ok(Value::Number(-num)),
                    _ => Err(BeemoError::RuntimeError(ErrorKind::NotNumber)),
                }
            }
            _ => Err(BeemoError::RuntimeError(ErrorKind::NotUnaryOperator)),
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
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number((l / r).floor())),
                _ => Err(BeemoError::RuntimeError(ErrorKind::WrongOperand)),
            },
            TokenType::EqualEqual => match (lexpr, rexpr) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l == r)),
                _ => Err(BeemoError::RuntimeError(ErrorKind::NotComparable)),
            },
            TokenType::GreaterThan => match (lexpr, rexpr) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l > r)),
                _ => Err(BeemoError::RuntimeError(ErrorKind::NotComparable)),
            },
            TokenType::GreaterEqual => match (lexpr, rexpr) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l >= r)),
                _ => Err(BeemoError::RuntimeError(ErrorKind::NotComparable)),
            },
            TokenType::LessThan => match (lexpr, rexpr) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l < r)),
                _ => Err(BeemoError::RuntimeError(ErrorKind::NotComparable)),
            },
            TokenType::LessEqual => match (lexpr, rexpr) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l <= r)),
                _ => Err(BeemoError::RuntimeError(ErrorKind::NotComparable)),
            },
            TokenType::Modulo => match (lexpr, rexpr) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l % r)),
                _ => Err(BeemoError::RuntimeError(ErrorKind::NotComparable)),
            },
            _ => Err(BeemoError::RuntimeError(ErrorKind::NotInfixOperator)),
        }
    }

    fn eval_assignment_expr(
        &self,
        lvalue: String,
        rvalue: Expr,
        env: &Environment,
    ) -> Result<Value> {
        let evaluated = self.eval_expr(rvalue, env)?;
        env.define(lvalue, evaluated.clone());
        Ok(evaluated)
    }

    fn eval_index_access_expr(
        &self,
        target: String,
        expr: Expr,
        env: &Environment,
    ) -> Result<Value> {
        let array = env
            .get(&target)
            .ok_or(BeemoError::RuntimeError(ErrorKind::VariableUndefined))?;
        match array {
            Value::Array(vec) => {
                let index = self.eval_expr(expr, env)?;
                match index {
                    Value::Number(num) => {
                        if num >= 0f32 {
                            let n = num as usize;
                            Ok(Value::Number(vec[n]))
                        } else {
                            let n = (vec.len() as f32 + num) as usize;
                            Ok(Value::Number(vec[n]))
                        }
                    }
                    _ => Err(BeemoError::RuntimeError(ErrorKind::CannotBeIndexedBy)),
                }
            }
            _ => Err(BeemoError::RuntimeError(ErrorKind::NotIndexable)),
        }
    }

    fn eval_push_expr(&self, array: String, expr: Expr, env: &Environment) -> Result<Value> {
        let val = self.eval_expr(expr, env)?;
        env.vec_push(&array, val.clone())
            .ok_or(BeemoError::RuntimeError(ErrorKind::VariableUndefined))?;
        Ok(val)
    }

    fn eval_push_var_expr(&self, array: String, var: Value, env: &Environment) -> Result<Value> {
        env.vec_push(&array, var.clone())
            .ok_or(BeemoError::RuntimeError(ErrorKind::VariableUndefined))?;
        Ok(var)
    }

    pub fn eval_expr(&self, expr: Expr, env: &Environment) -> Result<Value> {
        match expr {
            Expr::Variable(name) => self.eval_var_expr(name, env),
            Expr::Push(array, expr) => self.eval_push_expr(array, *expr, env),
            Expr::IndexAccess(target, expr) => self.eval_index_access_expr(target, *expr, env),
            Expr::Call(callee, args) => self.eval_call_expr(callee, args, env),
            Expr::Literal(value) => Ok(value),
            Expr::Assignment(lvalue, rvalue) => self.eval_assignment_expr(lvalue, *rvalue, env),
            Expr::Grouping(expr) => self.eval_expr(*expr, env),
            Expr::Unary(op, expr) => self.eval_unary_expr(op, *expr, env),
            Expr::Binary(op, lhs, rhs) => self.eval_binary_expr(op, *lhs, *rhs, env),
            Expr::Logical(op, lhs, rhs) => self.eval_logical_expr(op, *lhs, *rhs, env),
        }
    }

    pub fn eval_block(&self, stmts: Vec<Stmt>, env: &Environment) -> Result<()> {
        let length = stmts.len();
        for (i, stmt) in stmts.into_iter().enumerate() {
            let is_last = i == length - 1; // Function block.
            match stmt {
                Stmt::Expression(e @ Expr::Variable(_)) if is_last => {
                    let result = self.eval_expr(e, &env)?;
                    env.define("return".to_string(), result);
                    return Ok(());
                }
                Stmt::Expression(e @ Expr::Literal(_)) if is_last => {
                    let result = self.eval_expr(e, &env)?;
                    env.define("return".to_string(), result);
                    return Ok(());
                }
                Stmt::Return(expr) => {
                    let expr = self.eval_expr(expr, &env)?;
                    env.define("return".to_string(), expr);
                    return Ok(());
                }
                stmt if is_last => {
                    self.eval_stmt(stmt, &env)?;
                    env.define("unit_return".to_string(), Value::Unit);
                    return Ok(());
                }
                _ => self.eval_stmt(stmt, &env)?,
            }
        }
        Ok(())
    }

    fn eval_while_stmt(&self, cond: Expr, body: Vec<Stmt>, env: &Environment) -> Result<()> {
        let mut condition = self.eval_expr(cond.clone(), env)?;
        while matches!(condition, Value::Bool(true)) {
            if env.get("return").is_some() {
                return Ok(());
            }
            self.eval_block(body.clone(), &env)?;
            condition = self.eval_expr(cond.clone(), &env)?;
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
            Stmt::While(cond, block) => self.eval_while_stmt(cond, block, env),
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
