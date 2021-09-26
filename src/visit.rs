use crate::parser::{Expr, Stmt};

pub trait Visitor {
    fn visit_stmt(&mut self, stmt: &Stmt);
    fn visit_expr(&mut self, expr: &Expr);
}
