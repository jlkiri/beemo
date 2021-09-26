use std::slice::Iter;
use std::{collections::HashMap, iter::Peekable};

use crate::function::Function;
use crate::scanner::TokenType;
use crate::{error::*, scanner::Token};

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

#[derive(Debug)]
struct Declaration;

#[derive(Debug)]
pub enum Stmt {
    Print(String),
    Return(Expr),
    FunctionDeclaration(Function),
}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f32),
    Callable(Function),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Value),
    Variable(String),
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedEof,
    UnexpectedToken,
    MissingClosingBracket,
    Internal,
    NeedColon,
    NeedIndent,
    NeedDedent,
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

    fn read_token_if_any_of(&mut self, types: &[TokenType]) -> Option<Token> {
        for ty in types {
            if self.check(ty) {
                return self.read_token();
            }
        }
        None
    }

    fn read_token_if_ident(&mut self) -> Option<String> {
        match self.tokens.peek() {
            Some(token) => match &token.ty {
                TokenType::Identifier(value) => {
                    self.read_token();
                    Some(value.clone())
                }
                _ => None,
            },
            _ => None,
        }
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

    fn peek_token(&mut self) -> Option<&Token> {
        self.tokens.peek().copied()
    }

    fn params(&mut self) -> Result<Vec<String>> {
        let mut parameters = vec![];
        while {
            if let Some(token) = self.read_token_if_ident() {
                dbg!("IDENTIFIER", &token);
                parameters.push(token);
            }

            self.read_token_if(&TokenType::Comma).is_some()
        } {
            // Do-while loop.
        }
        self.read_token_if(&TokenType::ClosingParen)
            .ok_or(BeemoError::ParseError(ErrorKind::MissingClosingBracket))?;
        Ok(parameters)
    }

    fn end_of_block(&mut self) -> bool {
        self.check(&TokenType::Eof) || self.check(&TokenType::Dedent)
    }

    fn block(&mut self) -> Result<Vec<Expr>> {
        self.read_token_if(&TokenType::Indent)
            .ok_or(BeemoError::ParseError(ErrorKind::NeedIndent))?;
        let mut exprs = vec![];
        while !self.end_of_block() {
            exprs.push(self.expression()?)
        }
        self.read_token_if(&TokenType::Dedent)
            .ok_or(BeemoError::ParseError(ErrorKind::NeedDedent))?;
        Ok(exprs)
    }

    fn function(&mut self) -> Result<Stmt> {
        let name = self
            .read_token_if_ident()
            .ok_or(BeemoError::ParseError(ErrorKind::UnexpectedToken))?;

        match self
            .read_token_if_any_of(&[TokenType::OpeningParen, TokenType::Colon])
            .map(|t| t.ty)
        {
            Some(TokenType::OpeningParen) => {
                let params = self.params()?;
                self.read_token_if(&TokenType::Colon)
                    .ok_or(BeemoError::ParseError(ErrorKind::NeedColon))?;
                let body = self.block()?;
                Ok(Stmt::FunctionDeclaration(Function { params, name, body }))
            }
            Some(TokenType::Colon) => {
                todo!()
            }
            _ => todo!(),
        }
    }

    fn comparison(&mut self) -> Result<Expr> {
        self.term()
    }

    fn term(&mut self) -> Result<Expr> {
        self.factor()
    }

    fn factor(&mut self) -> Result<Expr> {
        self.unary()
    }

    fn unary(&mut self) -> Result<Expr> {
        self.call()
    }

    fn call(&mut self) -> Result<Expr> {
        self.literal()
    }

    fn literal(&mut self) -> Result<Expr> {
        if let Some(v) = self.read_token_if_float() {
            return Ok(Expr::Literal(Value::Number(v)));
        }

        match self.read_token_if_ident() {
            Some(v) => Ok(Expr::Variable(v)),
            _ => Err(BeemoError::ParseError(ErrorKind::UnexpectedToken)),
        }
    }

    fn equality(&mut self) -> Result<Expr> {
        self.comparison()
    }

    fn expression(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn declaration(&mut self) -> Result<Stmt> {
        self.function()
    }

    pub fn parse(&mut self) -> Result<Stmt> {
        self.declaration()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fn() {}
}
