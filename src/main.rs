#![allow(warnings)]

mod env;
mod error;
mod function;
mod interpreter;
mod parser;
mod scanner;

use error::BeemoError;
use interpreter::*;
use miette::{NamedSource, Result};
use parser::*;
use scanner::*;
use std::fs;
fn main() -> Result<()> {
    // As of now, final newline is REQUIRED.
    // let source = include_str!("../call.bmo");
    let file = std::env::args().nth(1).unwrap_or_else(|| {
        println!("Specify file to execute.");
        std::process::exit(1);
    });
    let source = fs::read_to_string(file).expect("Failed reading file.");
    let tokens = scan(&source)?;
    // dbg!(&tokens);
    let mut parser = Parser::new(tokens.iter().peekable());
    let res = parser.parse()?;
    let interpreter = Interpreter::new();
    interpreter.interpret(res)?;
    Ok(())
}
