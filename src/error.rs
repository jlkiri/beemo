use thiserror::Error;

use crate::interpreter;
use crate::parser;
use crate::scanner;

pub type Result<T> = std::result::Result<T, BeemoError>;

#[derive(Debug, Error)]
pub enum BeemoError {
    #[error("Parse error.")]
    ParseError(parser::ErrorKind),
    #[error("Scan error.")]
    ScanError(Vec<(String, scanner::ErrorKind)>),
    #[error("Runtime error.")]
    RuntimeError(interpreter::ErrorKind),
    #[error("Internal error.")]
    InternalError,
}
