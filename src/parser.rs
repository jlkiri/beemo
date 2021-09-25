use std::slice::Iter;
use std::{collections::HashMap, iter::Peekable};

use crate::scanner::TokenType;
use crate::{error::BeemoError, scanner::Token};

pub type Result<T> = std::result::Result<T, BeemoError>;

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

#[derive(Debug)]
struct Declaration;

#[derive(Debug)]
pub struct Function {
    params: Vec<String>,
    name: String,
}

enum Stmt {
    Print(String),
    Return(Expr),
}

enum Value {
    String,
    Number,
}

enum Expr {
    Literal(Value),
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedEof,
    UnexpectedToken,
    MissingClosingBracket,
    Internal,
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

    fn function(&mut self) -> Result<Function> {
        let name = self
            .read_token_if_ident()
            .ok_or(BeemoError::ParseError(ErrorKind::UnexpectedToken))?;

        match self
            .read_token_if_any_of(&[TokenType::OpeningParen, TokenType::Colon])
            .map(|t| t.ty)
        {
            Some(TokenType::OpeningParen) => {
                let params = self.params()?;
                Ok(Function { params, name })
            }
            Some(TokenType::Colon) => {
                todo!()
            }
            _ => todo!(),
        }
    }

    fn declaration(&mut self) -> Result<Function> {
        self.function()
    }

    pub fn parse(&mut self) -> Result<Function> {
        self.declaration()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fn() {}
}
