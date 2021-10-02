use miette::{self, Diagnostic, SourceSpan};
use thiserror::Error;

use crate::interpreter;
use crate::parser;
use crate::scanner;

pub type Result<T> = std::result::Result<T, BeemoError>;
type TokenLength = usize;
type Span = (usize, TokenLength);
type Help = String;
type Description = String;

#[derive(Debug, Error, Diagnostic)]
pub enum BeemoError {
    #[error(transparent)]
    ParseError(parser::ErrorKind),
    #[error("Scan error: {2}.")]
    #[diagnostic(code(beemo::scanner), help("{3}"))]
    ScanError(
        #[source_code] String,
        #[label = "Here."] Span,
        Description,
        Help,
    ),
    /* #[error("Scan error: {1}.")]
    #[diagnostic(code(beemo::scanner), help("{0}"))]
    ScanError(
        String,
        String,
        #[source_code] String,
        #[label = "Here."] (SpanStart, SpanEnd),
    ), */
    #[error("Runtime error.")]
    RuntimeError(interpreter::ErrorKind),
    #[error("Internal error.")]
    InternalError,
}
