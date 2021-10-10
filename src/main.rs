mod env;
mod error;
mod function;
mod interpreter;
mod parser;
mod scanner;

use interpreter::*;
use miette::Result;
use parser::*;
use scanner::*;
use std::fs;
fn main() -> Result<()> {
    miette::set_hook(Box::new(|_| {
        Box::new(miette::MietteHandlerOpts::new().tab_width(4).build())
    }))
    .unwrap();

    // As of now, final newline is REQUIRED.
    let file = std::env::args().nth(1).unwrap_or_else(|| {
        println!("Specify file to execute.");
        std::process::exit(1);
    });
    let source = fs::read_to_string(file).expect("Failed reading file.");
    let tokens = scan(&source)?;
    // dbg!(&tokens);
    let mut parser = Parser::new(&source, tokens.iter().peekable());
    let res = parser.parse()?;
    let mut interpreter = Interpreter::new();
    interpreter.interpret(res).expect("derp");
    Ok(())
}
