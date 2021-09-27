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
    Call(String, Vec<Expr>),
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedEof,
    UnexpectedToken,
    BadLiteral,
    MissingClosingBracket,
    Internal,
    NeedColon,
    NeedIndent,
    NeedDedent,
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

    /* fn check_keyword(&mut self, value: String) -> bool {
        if let Some(token) = self.tokens.peek() {
            &token.ty == ty
        } else {
            false
        }
    } */

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
                    return Some(value.clone());
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn read_token_if_keyword(&mut self) -> Option<String> {
        match self.tokens.peek() {
            Some(token) => match &token.ty {
                TokenType::Keyword(kw) => {
                    self.read_token();
                    return Some(kw.clone());
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
                dbg!("param");
                dbg!(&token);
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

    fn block(&mut self) -> Result<Vec<Stmt>> {
        self.read_token_if(&TokenType::Indent)
            .ok_or(BeemoError::ParseError(ErrorKind::NeedIndent))?;
        let mut stmts = vec![];
        while !self.end_of_block() {
            stmts.push(self.statement()?)
        }
        self.read_token_if(&TokenType::Dedent)
            .ok_or(BeemoError::ParseError(ErrorKind::NeedDedent))?;
        Ok(stmts)
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
                let body = self.block()?;
                Ok(Stmt::FunctionDeclaration(Function {
                    params: vec![],
                    name,
                    body,
                }))
            }
            _ => Err(BeemoError::InternalError),
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
        let lit = self.literal()?;
        if self.read_token_if(&TokenType::OpeningParen).is_some() {
            let mut arguments = vec![];
            while {
                let arg = self.expression()?;
                arguments.push(arg);
                self.read_token_if(&TokenType::Comma).is_some()
            } {
                // Do-while loop.
            }
            self.read_token_if(&TokenType::ClosingParen)
                .ok_or(BeemoError::ParseError(ErrorKind::MissingClosingBracket))?;
            match lit {
                Expr::Variable(callee) => Ok(Expr::Call(callee, arguments)),
                _ => Err(BeemoError::ParseError(ErrorKind::BadCall)),
            }
        } else {
            Ok(lit)
        }
    }

    fn literal(&mut self) -> Result<Expr> {
        if let Some(v) = self.read_token_if_float() {
            return Ok(Expr::Literal(Value::Number(v)));
        }

        match self.read_token_if_ident() {
            Some(ref v) => Ok(Expr::Variable(v.clone())),
            None => Err(BeemoError::ParseError(ErrorKind::BadLiteral)),
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
