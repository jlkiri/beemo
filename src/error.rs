use miette::{self, Diagnostic, SourceSpan};
use thiserror::Error;

use crate::interpreter;
use crate::parser;
use crate::scanner;

pub type Result<T> = std::result::Result<T, BeemoError>;

#[derive(Debug, Error, Diagnostic)]
pub enum BeemoError {
    #[error("Parse error.")]
    ParseError(parser::ErrorKind),
    #[error("Scan error: {2}.")]
    #[diagnostic(code(beemo::scanner))]
    ScanError(
        #[source_code] String,
        #[label = "This is the highlight"] (usize, usize),
        scanner::ErrorKind,
    ),
    #[error("Runtime error.")]
    RuntimeError(interpreter::ErrorKind),
    #[error("Internal error.")]
    InternalError,
}
