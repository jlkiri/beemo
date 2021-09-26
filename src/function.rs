use crate::parser::Expr;

#[derive(Debug)]
pub struct Function {
    pub params: Vec<String>,
    pub name: String,
    pub body: Vec<Expr>,
}
