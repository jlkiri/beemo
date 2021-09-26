mod env;
mod error;
mod function;
mod interpreter;
mod parser;
mod scanner;

use error::*;
use parser::*;
use scanner::*;

fn main() {
    // As of now, final newline is REQUIRED.
    let source = include_str!("../call.bmo");
    let tokens = scan(&source).unwrap();
    dbg!(&tokens);
    let mut parser = Parser::new(tokens.iter().peekable());
    let res = parser.parse().unwrap();
    dbg!(res);
    ()
}
