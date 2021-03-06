use std::iter::{Enumerate, Peekable};
use std::time::Instant;
use std::vec::IntoIter;

use crate::function::{Callable, Function};
use crate::scanner::TokenType;
use crate::{error::*, scanner::Token};
use miette::Diagnostic;
use thiserror::Error;

pub struct Parser<'a> {
    source: &'a str,
    tokens: Peekable<Enumerate<IntoIter<Token>>>,
    token_source: Vec<Token>,
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
    Callable(Box<dyn Callable>),
    Instant(Instant),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Value),
    Push(String, Box<Expr>),
    IndexAccess(String, Box<Expr>),
    Grouping(Box<Expr>),
    Assignment(String, Box<Expr>),
    Unary(TokenType, Box<Expr>),
    Variable(Token, String),
    Logical(TokenType, Box<Expr>, Box<Expr>),
    Binary(TokenType, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
}

#[derive(Debug, Error, Diagnostic)]
#[error("Parser error.")]
pub enum ErrorKind {
    #[error("Unexpected EOF (end of file).")]
    #[diagnostic(help("Does your file end with a newline?"))]
    UnexpectedEof,
    #[error("Unexpected token.")]
    #[diagnostic(help("Not much to say."))]
    UnexpectedToken,
    #[error("Invalid index target.")]
    #[diagnostic(help("This type cannot be indexed. Are you sure this is an array?"))]
    InvalidIndexTarget,
    #[error("Invalid assignment target.")]
    #[diagnostic(help("Only variables (but not other expressions) can be assigned a value."))]
    NotAssignable,
    #[error("Expected a function name.")]
    #[diagnostic(help("All function must have a unique name."))]
    ExpectedFunctionName,
    #[error("Expected an argument.")]
    #[diagnostic(help("Only arguments can appear in this position."))]
    ExpectedArgument,
    #[error("Expected a number.")]
    #[diagnostic(help("Only numbers can appear in this position."))]
    ExpectedNumber,
    #[error("Not a literal.")]
    #[diagnostic(help("Only literals can appear in this position."))]
    BadLiteral,
    #[error("Missing closing bracket.")]
    #[diagnostic(help("Missing closing bracket."))]
    MissingClosingBracket,
    #[error("Missing closing parenthesis.")]
    #[diagnostic(help("Missing closing parenthesis."))]
    MissingClosingParen,
    #[error("Missing closing brace.")]
    #[diagnostic(help("Missing closing brace."))]
    MissingClosingBrace,
    #[error("Missing closing parenthesis.")]
    #[diagnostic(help("Missing closing parenthesis."))]
    MissingClosingParenInCall,
    #[error("Internal parser error.")]
    #[diagnostic(help("There is likely to be a bug in the parser."))]
    Internal,
    #[error("Missing colon.")]
    #[diagnostic(help("Missing colon."))]
    ExpectedColon,
    #[error("Bad indentation.")]
    #[diagnostic(help("Expected indendation."))]
    ExpectedIndent,
    #[error("Bad indentation.")]
    #[diagnostic(help("Expected dedent."))]
    ExpectedDedent,
    #[error("Bad call.")]
    #[diagnostic(help("This expression is not callable."))]
    BadCall,
    #[error("Bad push.")]
    #[diagnostic(help("This expression cannot be pushed to."))]
    BadPush,
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
    pub fn new(source: &'a str, token_source: Vec<Token>) -> Self {
        let tokens = token_source.clone().into_iter().enumerate().peekable();
        Self {
            source,
            tokens,
            token_source,
        }
    }

    fn check(&mut self, ty: &TokenType) -> bool {
        if let Some(token) = self.tokens.peek().map(|t| t.1.clone()) {
            &token.ty == ty
        } else {
            false
        }
    }

    fn read_token(&mut self) -> Option<Token> {
        self.tokens.next().map(|t| t.1)
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

    fn read_token_if_ident(&mut self) -> Option<(Token, String)> {
        self.tokens
            .next_if(|t| matches!(t.1.ty, TokenType::Identifier(..)))
            .and_then(|t| {
                let val = t.1.ty.string_value().unwrap().to_string();
                Some((t.1, val))
            })
    }

    fn read_token_if_string(&mut self) -> Option<String> {
        self.tokens
            .next_if(|t| matches!(t.1.ty, TokenType::String(_)))
            .and_then(|t| match t.1 {
                Token { ty, .. } => Some(ty.string_value().unwrap().to_string()), // Safe unwrap.
            })
    }

    fn read_token_if_keyword(&mut self) -> Option<String> {
        self.tokens
            .next_if(|t| matches!(t.1.ty, TokenType::Keyword(_)))
            .and_then(|t| match t.1 {
                Token { ty, .. } => Some(ty.string_value().unwrap().to_string()), // Safe unwrap.
            })
    }

    fn read_token_if_float(&mut self) -> Option<f32> {
        self.tokens
            .next_if(|t| matches!(t.1.ty, TokenType::Float(_)))
            .and_then(|t| match t.1 {
                Token { ty, .. } => Some(ty.num_value().unwrap()), // Safe unwrap.
            })
    }

    fn params(&mut self) -> Result<Vec<String>> {
        let mut parameters = vec![];
        while {
            if let Some((_, token)) = self.read_token_if_ident() {
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

    fn err_on_prev(&mut self, kind: ErrorKind) -> BeemoError {
        let prev = self.previous().unwrap();
        BeemoError::ParserError(
            self.source.to_string(),
            prev.span,
            kind.to_string(),
            kind.help().unwrap().to_string(),
        )
    }

    fn err(&mut self, kind: ErrorKind) -> BeemoError {
        let prev = self.peek_token().unwrap();
        BeemoError::ParserError(
            self.source.to_string(),
            prev.span,
            kind.to_string(),
            kind.help().unwrap().to_string(),
        )
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
        let (_, name) = self
            .read_token_if_ident()
            .ok_or(self.err(ErrorKind::ExpectedFunctionName))?;

        match self
            .read_token_if_any_of(&[TokenType::OpeningParen, TokenType::Colon])
            .map(|t| t.ty)
        {
            Some(TokenType::OpeningParen) => {
                let params = self.params()?;
                self.read_token_if(&TokenType::Colon)
                    .ok_or(self.err_on_prev(ErrorKind::ExpectedColon))?;
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
            _ => Err(self.err_on_prev(ErrorKind::ExpectedColon)),
        }
    }

    fn term_finish(&mut self, min_bp: u8) -> Result<Expr> {
        let mut lhs = self.unary()?;
        loop {
            let op = match self.peek_token() {
                Some(Token { ty, .. }) if self.is_op(&ty) => ty,
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
        self.tokens.peek().map(|t| t.1.clone())
    }

    fn previous(&mut self) -> Option<Token> {
        let (i, _) = self.tokens.peek().unwrap().clone();
        let prev = self.token_source[i - 1].clone();
        Some(prev)
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
            Expr::Variable(_, target) => target,
            _ => Err(self.err(ErrorKind::InvalidIndexTarget))?,
        };
        Ok(Expr::IndexAccess(target, Box::new(member)))
    }

    fn call(&mut self, lit: Expr) -> Result<Expr> {
        let callee = match lit {
            Expr::Variable(_, callee) => callee,
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
            .ok_or(self.err_on_prev(ErrorKind::MissingClosingParenInCall))?;
        Ok(Expr::Call(callee, arguments))
    }

    fn literal(&mut self) -> Result<Expr> {
        // TODO: Make push (>>) parsing cleaner.
        if let Some(v) = self.read_token_if_float() {
            if self.read_token_if(&TokenType::Push).is_some() {
                let (_, array) = self
                    .read_token_if_ident()
                    .ok_or(self.err(ErrorKind::BadPush))?;
                return Ok(Expr::Push(array, Box::new(Expr::Literal(Value::Number(v)))));
            }
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
                .ok_or(self.err_on_prev(ErrorKind::MissingClosingParen))?;
            return Ok(Expr::Grouping(Box::new(group)));
        }

        match self.read_token_if_ident() {
            Some((t, val)) => {
                if self.read_token_if(&TokenType::Push).is_some() {
                    let (_, array) = self
                        .read_token_if_ident()
                        .ok_or(self.err(ErrorKind::BadPush))?;
                    return Ok(Expr::Push(array, Box::new(Expr::Variable(t, val))));
                }
                Ok(Expr::Variable(t, val))
            }
            None => Err(self.err(ErrorKind::BadLiteral)),
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
            let assignee = self.expression()?;
            match assignee {
                Expr::Variable(_, var) => Ok(Expr::Assignment(var, Box::new(rvalue))),
                _ => Err(self.err_on_prev(ErrorKind::NotAssignable))?,
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
                ..
            }) if k == "else" => {
                self.read_token();
                self.read_token_if(&TokenType::Colon)
                    .ok_or(self.err_on_prev(ErrorKind::ExpectedColon))?;
                let block = self.block()?;
                Ok(Some(block))
            }
            _ => Ok(None),
        }
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.read_token_if(&TokenType::Colon)
            .ok_or(self.err_on_prev(ErrorKind::ExpectedColon))?;
        let if_branch = self.block()?;
        let else_branch = self.else_branch()?;
        Ok(Stmt::Condition(expr, if_branch, else_branch))
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        let condition = self.expression()?;
        self.read_token_if(&TokenType::Colon)
            .ok_or(self.err_on_prev(ErrorKind::ExpectedColon))?;
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
