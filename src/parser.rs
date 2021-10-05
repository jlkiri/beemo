use std::iter::Peekable;
use std::slice::Iter;

use crate::function::Function;
use crate::scanner::TokenType;
use crate::{error::*, scanner::Token};

use thiserror::Error;

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

type IfBranch = Vec<Stmt>;
type ElseBranch = Vec<Stmt>;

#[derive(Debug, Clone)]
pub enum Stmt {
    Print(Expr),
    Condition(Expr, IfBranch, Option<ElseBranch>),
    Return(Expr),
    FunctionDeclaration(Function),
    While(Expr, Vec<Stmt>),
    Expression(Expr),
}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f32),
    Array(Vec<f32>),
    Callable(Function),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Value),
    IndexAccess(String, Box<Expr>),
    Grouping(Box<Expr>),
    Assignment(String, Box<Expr>),
    Unary(TokenType, Box<Expr>),
    Variable(String),
    Logical(TokenType, Box<Expr>, Box<Expr>),
    Binary(TokenType, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedEof,
    UnexpectedToken,
    InvalidIndexTarget,
    NotAssignable,
    ExpectedFunctionName,
    ExpectedArgument,
    ExpectedNumber,
    BadLiteral(Option<Token>),
    MissingClosingBracket,
    MissingClosingParen,
    MissingClosingBrace,
    MissingClosingParenInCall(Token),
    Internal,
    ExpectedColon,
    ExpectedIndent,
    ExpectedDedent,
    BadCall,
}

impl Value {
    pub fn is_bool(&self) -> bool {
        match self {
            Value::Bool(..) => true,
            _ => false,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Peekable<Iter<'a, Token>>) -> Self {
        Self { tokens }
    }

    fn check(&mut self, ty: &TokenType) -> bool {
        if let Some(token) = self.tokens.peek() {
            &token.ty == ty
        } else {
            false
        }
    }

    fn read_token(&mut self) -> Option<Token> {
        self.tokens.next().cloned()
    }

    fn read_token_if(&mut self, ty: &TokenType) -> Option<Token> {
        if self.check(ty) {
            return self.read_token();
        }
        None
    }

    fn is_op(&self, ty: &TokenType) -> bool {
        matches!(
            ty,
            TokenType::Plus
                | TokenType::Multiply
                | TokenType::Divide
                | TokenType::Minus
                | TokenType::Modulo
        )
    }

    fn read_token_if_any_of(&mut self, types: &[TokenType]) -> Option<Token> {
        for ty in types {
            if self.check(ty) {
                return self.read_token();
            }
        }
        None
    }

    fn read_token_if_ident(&mut self) -> Option<String> {
        self.tokens
            .next_if(|t| matches!(t.ty, TokenType::Identifier(..)))
            .and_then(|t| match t {
                Token { ty } => {
                    Some(ty.string_value().unwrap().to_string()) // Safe unwrap.
                }
                _ => None,
            })
    }

    fn read_token_if_string(&mut self) -> Option<String> {
        self.tokens
            .next_if(|t| matches!(t.ty, TokenType::String(_)))
            .and_then(|t| match t {
                Token { ty } => Some(ty.string_value().unwrap().to_string()), // Safe unwrap.
                _ => None,
            })
    }

    fn read_token_if_keyword(&mut self) -> Option<String> {
        self.tokens
            .next_if(|t| matches!(t.ty, TokenType::Keyword(_)))
            .and_then(|t| match t {
                Token { ty } => Some(ty.string_value().unwrap().to_string()), // Safe unwrap.
                _ => None,
            })
    }

    fn read_token_if_float(&mut self) -> Option<f32> {
        match self.tokens.peek() {
            Some(token) => match &token.ty {
                TokenType::Float(value) => {
                    self.read_token();
                    Some(value.clone())
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn params(&mut self) -> Result<Vec<String>> {
        let mut parameters = vec![];
        while {
            if let Some(token) = self.read_token_if_ident() {
                parameters.push(token);
            }

            self.read_token_if(&TokenType::Comma).is_some()
        } {
            // Do-while loop.
        }
        self.read_token_if(&TokenType::ClosingParen)
            .ok_or(self.err(ErrorKind::MissingClosingBracket))?;
        Ok(parameters)
    }

    fn end_of_block(&mut self) -> bool {
        self.check(&TokenType::Eof) || self.check(&TokenType::Dedent)
    }

    fn err(&self, kind: ErrorKind) -> BeemoError {
        BeemoError::ParseError(kind)
    }

    fn block(&mut self) -> Result<Vec<Stmt>> {
        self.read_token_if(&TokenType::Indent)
            .ok_or(self.err(ErrorKind::ExpectedIndent))?;
        let mut stmts = vec![];
        while !self.end_of_block() {
            stmts.push(self.statement()?)
        }
        self.read_token_if(&TokenType::Dedent)
            .ok_or(self.err(ErrorKind::ExpectedDedent))?;
        Ok(stmts)
    }

    fn function(&mut self) -> Result<Stmt> {
        let name = self
            .read_token_if_ident()
            .ok_or(self.err(ErrorKind::ExpectedFunctionName))?;

        match self
            .read_token_if_any_of(&[TokenType::OpeningParen, TokenType::Colon])
            .map(|t| t.ty)
        {
            Some(TokenType::OpeningParen) => {
                let params = self.params()?;
                self.read_token_if(&TokenType::Colon)
                    .ok_or(self.err(ErrorKind::ExpectedColon))?;
                let body = self.block()?;
                Ok(Stmt::FunctionDeclaration(Function { params, name, body }))
            }
            Some(TokenType::Colon) => {
                let body = self.block()?;
                Ok(Stmt::FunctionDeclaration(Function {
                    params: vec![],
                    name,
                    body,
                }))
            }
            _ => Err(self.err(ErrorKind::ExpectedColon)),
        }
    }

    fn term_finish(&mut self, min_bp: u8) -> Result<Expr> {
        let mut lhs = self.unary()?;
        loop {
            let op = match self.peek_token() {
                Some(Token { ty }) if self.is_op(&ty) => ty,
                _ => break,
            };
            let (lbp, rbp) = self.infix_binding_power(&op);
            if lbp < min_bp {
                break;
            }
            self.read_token();
            let rhs = self.term_finish(rbp)?;
            lhs = Expr::Binary(op, Box::new(lhs), Box::new(rhs))
        }
        Ok(lhs)
    }

    fn term(&mut self) -> Result<Expr> {
        self.term_finish(0)
    }

    fn infix_binding_power(&self, op: &TokenType) -> (u8, u8) {
        match op {
            TokenType::Divide | TokenType::Multiply => (4, 5),
            TokenType::Plus | TokenType::Minus => (2, 3),
            TokenType::Modulo => (1, 2),
            _ => todo!(),
        }
    }

    fn unary(&mut self) -> Result<Expr> {
        if let Some(t) = self.read_token_if_any_of(&[TokenType::Bang, TokenType::Minus]) {
            let expr = self.unary()?;
            return Ok(Expr::Unary(t.ty, Box::new(expr)));
        }
        self.call_like()
    }

    fn peek_token(&mut self) -> Option<Token> {
        self.tokens.peek().copied().cloned()
    }

    fn call_like(&mut self) -> Result<Expr> {
        let lit = self.literal()?;
        if self.read_token_if(&TokenType::OpeningParen).is_some() {
            return self.call(lit);
        }
        if self.read_token_if(&TokenType::OpeningBracket).is_some() {
            return self.index_access(lit);
        }
        Ok(lit)
    }

    fn index_access(&mut self, lit: Expr) -> Result<Expr> {
        let member = self.term()?;
        self.read_token_if(&TokenType::ClosingBracket)
            .ok_or(self.err(ErrorKind::MissingClosingBracket))?;
        let target = match lit {
            Expr::Variable(target) => target,
            _ => Err(self.err(ErrorKind::InvalidIndexTarget))?,
        };
        Ok(Expr::IndexAccess(target, Box::new(member)))
    }

    fn call(&mut self, lit: Expr) -> Result<Expr> {
        let callee = match lit {
            Expr::Variable(callee) => callee,
            _ => Err(self.err(ErrorKind::UnexpectedToken))?,
        };

        let mut arguments = vec![];
        if self.read_token_if(&TokenType::ClosingParen).is_some() {
            return Ok(Expr::Call(callee, arguments));
        }
        while {
            let arg = self
                .expression()
                .or(Err(self.err(ErrorKind::ExpectedArgument)))?;
            arguments.push(arg);
            self.read_token_if(&TokenType::Comma).is_some()
        } {
            // Do-while loop.
        }
        self.read_token_if(&TokenType::ClosingParen)
            .ok_or_else(|| {
                let t = self.peek_token().expect("Unexpected eof.");
                self.err(ErrorKind::MissingClosingParenInCall(t))
            })?;
        Ok(Expr::Call(callee, arguments))
    }

    fn literal(&mut self) -> Result<Expr> {
        if let Some(v) = self.read_token_if_float() {
            return Ok(Expr::Literal(Value::Number(v)));
        }

        if let Some(s) = self.read_token_if_string() {
            return Ok(Expr::Literal(Value::String(s)));
        }

        if let Some(bool) = self.read_token_if_keyword() {
            match bool.as_str() {
                "true" => return Ok(Expr::Literal(Value::Bool(true))),
                "false" => return Ok(Expr::Literal(Value::Bool(false))),
                _ => return Err(self.err(ErrorKind::UnexpectedToken)),
            }
        }

        if self.read_token_if(&TokenType::OpeningBrace).is_some() {
            return self.array();
        }

        if self.read_token_if(&TokenType::OpeningParen).is_some() {
            let group = self.expression()?;
            self.read_token_if(&TokenType::ClosingParen)
                .ok_or(self.err(ErrorKind::MissingClosingParen))?;
            return Ok(Expr::Grouping(Box::new(group)));
        }

        let p = self.peek_token();

        match self.read_token_if_ident() {
            Some(v) => Ok(Expr::Variable(v)),
            None => Err(self.err(ErrorKind::BadLiteral(p))),
        }
    }

    fn array(&mut self) -> Result<Expr> {
        let mut members = vec![];
        if self.read_token_if(&TokenType::ClosingBrace).is_some() {
            return Ok(Expr::Literal(Value::Array(members)));
        }
        while {
            let num = self.literal().map(|expr| match expr {
                Expr::Literal(Value::Number(f)) => Ok(f),
                _ => Err(self.err(ErrorKind::ExpectedArgument)),
            })??;
            members.push(num);
            self.read_token_if(&TokenType::Comma).is_some()
        } {
            // Do-while loop.
        }
        self.read_token_if(&TokenType::ClosingBrace)
            .ok_or(self.err(ErrorKind::MissingClosingBrace))?;
        Ok(Expr::Literal(Value::Array(members)))
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut left = self.term()?;
        while let Some(token) = self.read_token_if_any_of(&[
            TokenType::GreaterThan,
            TokenType::LessThan,
            TokenType::LessEqual,
            TokenType::GreaterEqual,
        ]) {
            let right = self.term()?;
            left = Expr::Binary(token.ty, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut left = self.comparison()?;
        while let Some(token) = self.read_token_if(&TokenType::EqualEqual) {
            let right = self.comparison()?;
            left = Expr::Binary(token.ty, Box::new(left), Box::new(right.clone()));
        }
        Ok(left)
    }

    fn logical(&mut self) -> Result<Expr> {
        let mut left = self.equality()?;
        while let Some(token) = self.read_token_if_any_of(&[
            TokenType::Keyword("and".to_string()),
            TokenType::Keyword("or".to_string()),
        ]) {
            let right = self.equality()?;
            left = Expr::Logical(token.ty, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn assignment(&mut self) -> Result<Expr> {
        let rvalue = self.logical()?;
        if self.read_token_if(&TokenType::Assign).is_some() {
            match self.read_token_if_ident() {
                Some(lvalue) => Ok(Expr::Assignment(lvalue, Box::new(rvalue))),
                None => Err(self.err(ErrorKind::NotAssignable))?,
            }
        } else {
            Ok(rvalue)
        }
    }

    fn expression(&mut self) -> Result<Expr> {
        let expr = self.assignment()?;
        Ok(expr)
    }

    fn else_branch(&mut self) -> Result<Option<Vec<Stmt>>> {
        match self.peek_token() {
            Some(Token {
                ty: TokenType::Keyword(k),
            }) if k == "else" => {
                self.read_token();
                self.read_token_if(&TokenType::Colon)
                    .ok_or(self.err(ErrorKind::ExpectedColon))?;
                let block = self.block()?;
                Ok(Some(block))
            }
            _ => Ok(None),
        }
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.read_token_if(&TokenType::Colon)
            .ok_or(self.err(ErrorKind::ExpectedColon))?;
        let if_branch = self.block()?;
        let else_branch = self.else_branch()?;
        Ok(Stmt::Condition(expr, if_branch, else_branch))
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        let condition = self.expression()?;
        self.read_token_if(&TokenType::Colon)
            .ok_or(self.err(ErrorKind::ExpectedColon))?;
        let block = self.block()?;
        Ok(Stmt::While(condition, block))
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        Ok(Stmt::Return(expr))
    }

    fn statement(&mut self) -> Result<Stmt> {
        match self.read_token_if_keyword() {
            Some(kw) => match kw.as_str() {
                "print" => Ok(Stmt::Print(self.expression()?)),
                "if" => self.if_statement(),
                "while" => self.while_statement(),
                "return" => self.return_statement(),
                _ => todo!(),
            },
            None => Ok(Stmt::Expression(self.expression()?)),
        }
    }

    fn declaration(&mut self) -> Result<Stmt> {
        self.function()
    }

    fn eof(&mut self) -> bool {
        self.check(&TokenType::Eof)
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut decls = vec![];
        while !self.eof() {
            decls.push(self.declaration()?)
        }
        Ok(decls)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fn() {}
}
