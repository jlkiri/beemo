use std::iter::Peekable;
use std::slice::Iter;

use crate::{error::BeemoError, scanner::Token};

pub struct Tokens<I> {
    inner: I,
}

impl<I: Iterator<Item = Token>> Iterator for Tokens<I> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

#[derive(Debug)]
struct Declaration;

#[derive(Debug)]
struct Function;

impl<'a> Parser<'a> {
    pub fn new(tokens: Peekable<Iter<'a, Token>>) -> Self {
        Self { tokens }
    }

    fn function(&mut self) -> Result<Function, BeemoError> {
        let token = self
            .tokens
            .next()
            .ok_or(BeemoError::ParseError("eof".into()))?;

        match token {
            Token::Identifier(txt) => Ok(Function),
            _ => Ok(Function),
        }
    }

    fn declaration(&mut self) -> Result<Declaration, BeemoError> {
        /* let token = self
        .tokens
        .next()
        .ok_or(BeemoError::ParseError("eof".into()))?; */
        self.function()?;

        Ok(Declaration)
    }

    pub fn parse(&mut self) -> Result<(), BeemoError> {
        self.declaration()?;
        Ok(())
    }
}
