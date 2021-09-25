use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::parser;
use crate::scanner;

#[derive(Debug, Error)]
pub enum BeemoError {
    #[error("Parse error.")]
    ParseError(parser::ErrorKind),
    #[error("Scan error.")]
    ScanError(Vec<(String, scanner::ErrorKind)>),
}
