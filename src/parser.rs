use std::iter::Peekable;
use std::slice::Iter;

use crate::function::Function;
use crate::scanner::TokenType;
use crate::{error::*, scanner::Token};

use thiserror::Error;

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Print(Expr),
    Return(Expr),
    FunctionDeclaration(Function),
    Expression(Expr),
}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f32),
    Callable(Function),
    Unit,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Value),
    Variable(String),
    Binary(TokenType, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
}

#[derive(Debug, Error)]
#[error("Whoops.")]
pub enum ErrorKind {
    UnexpectedEof,
    UnexpectedToken,
    ExpectedFunctionName,
    ExpectedArgument,
    BadLiteral,
    MissingClosingBracket,
    MissingClosingParenInCall(Token),
    Internal,
    ExpectedColon,
    ExpectedIndent,
    ExpectedDedent,
    BadCall,
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
            TokenType::Plus | TokenType::Multiply | TokenType::Divide | TokenType::Minus
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
            .next_if(|t| matches!(t.ty, TokenType::Identifier(_)))
            .and_then(|t| match t {
                Token { ty } => Some(ty.string_value().unwrap().to_string()), // Safe unwrap.
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

    fn comparison_finish(&mut self, min_bp: u8) -> Result<Expr> {
        let mut lhs = self.unary()?;
        loop {
            let op = match self.peek_token() {
                None => break,
                Some(Token { ty }) if self.is_op(&ty) => ty,
                _ => break,
            };
            let (lbp, rbp) = self.infix_binding_power(&op);
            if lbp < min_bp {
                break;
            }
            self.read_token();
            let rhs = self.comparison_finish(rbp)?;
            lhs = Expr::Binary(op, Box::new(lhs), Box::new(rhs))
        }
        Ok(lhs)
    }

    fn comparison(&mut self) -> Result<Expr> {
        self.comparison_finish(0)
    }

    fn infix_binding_power(&self, op: &TokenType) -> (u8, u8) {
        match op {
            TokenType::Divide | TokenType::Multiply => (3, 4),
            TokenType::Plus | TokenType::Minus => (1, 2),
            _ => todo!(),
        }
    }

    fn unary(&mut self) -> Result<Expr> {
        self.call()
    }

    fn peek_token(&mut self) -> Option<Token> {
        self.tokens.peek().copied().cloned()
    }

    fn call(&mut self) -> Result<Expr> {
        let lit = self.literal()?;
        if self.read_token_if(&TokenType::OpeningParen).is_some() {
            let mut arguments = vec![];
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
            match lit {
                Expr::Variable(callee) => Ok(Expr::Call(callee, arguments)),
                _ => Err(self.err(ErrorKind::UnexpectedToken)),
            }
        } else {
            Ok(lit)
        }
    }

    fn literal(&mut self) -> Result<Expr> {
        if let Some(v) = self.read_token_if_float() {
            return Ok(Expr::Literal(Value::Number(v)));
        }

        if let Some(s) = self.read_token_if_string() {
            return Ok(Expr::Literal(Value::String(s)));
        }

        match self.read_token_if_ident() {
            Some(v) => Ok(Expr::Variable(v)),
            None => Err(self.err(ErrorKind::BadLiteral)),
        }
    }

    fn equality(&mut self) -> Result<Expr> {
        self.comparison()
    }

    fn expression(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn statement(&mut self) -> Result<Stmt> {
        match self.read_token_if_keyword() {
            Some(kw) => match kw.as_str() {
                "print" => Ok(Stmt::Print(self.expression()?)),
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
