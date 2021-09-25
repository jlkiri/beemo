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

macro_rules! ttype {
    (Identifier) => {
        TokenType::Identifier(Default::default())
    };
    ($name:ident) => {
        TokenType::$name
    };
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

    fn consume(&mut self, ty: TokenType) -> Result<&Token> {
        let token = self
            .tokens
            .next()
            .ok_or(BeemoError::ParseError(ErrorKind::UnexpectedEof))?;
        if matches!(&token.ty, ty) {
            Ok(token)
        } else {
            Err(BeemoError::ParseError(ErrorKind::UnexpectedToken))
        }
    }

    fn function(&mut self) -> Result<Function> {
        let token = self
            .tokens
            .next()
            .ok_or(BeemoError::ParseError(ErrorKind::UnexpectedEof))?;
        let t = self.consume(ttype!(Identifier))?;
        Ok(Function)
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
    fn test_consume() {
        let tokens = vec![Token {
            ty: TokenType::Identifier(String::from("smth")),
        }];
        let mut parser = Parser::new(tokens.iter().peekable());
        assert!(parser.consume(ttype!(Identifier)).is_ok())
    }
}
