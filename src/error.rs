use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::ErrorKind;

#[derive(Debug, Error)]
pub enum BeemoError {
    #[error("Parse error: {0}.")]
    ParseError(String),
    #[error("Scan error.")]
    ScanError(Vec<(String, ErrorKind)>),
}
