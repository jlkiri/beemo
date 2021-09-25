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
struct Function;

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
    Internal,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Peekable<Iter<'a, Token>>) -> Self {
        Self { tokens }
    }

    fn params(&mut self) -> Result<()> {
        let token = self
            .tokens
            .next()
            .ok_or(BeemoError::ParseError(ErrorKind::UnexpectedEof))?;
        /*  match token {
            Token::OpeningParen => {
                let ps = HashMap::new();
            }
        } */
        Ok(())
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

    fn function(&mut self) -> Result<Function> {
        let token = self
            .read_token_if_ident()
            .ok_or(BeemoError::ParseError(ErrorKind::UnexpectedToken))?;

        match self.peek_token().map(|t| &t.ty) {
            Some(TokenType::OpeningParen) => {
                todo!()
            }
            Some(TokenType::Colon) => {
                todo!()
            }
            _ => todo!(),
        }
    }

    fn declaration(&mut self) -> Result<Declaration> {
        /* let token = self
        .tokens
        .next()
        .ok_or(BeemoError::ParseError("eof".into()))?; */
        self.function()?;

        Ok(Declaration)
    }

    pub fn parse(&mut self) -> Result<()> {
        self.declaration()?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_consume() {}
}
