use miette::{self, Diagnostic};
use thiserror::Error;

use crate::interpreter;

pub type Result<T> = std::result::Result<T, BeemoError>;
type Length = usize;
type Span = (usize, Length);
type Help = String;
type Description = String;

#[derive(Debug, Error, Diagnostic)]
pub enum BeemoError {
    #[error("Parser error: {2}")]
    #[diagnostic(code(beemo::parser), help("{3}"))]
    ParserError(
        #[source_code] String,
        #[label = "Here."] Span,
        Description,
        Help,
    ),
    #[error("Scan error: {2}.")]
    #[diagnostic(code(beemo::scanner), help("{3}"))]
    ScanError(
        #[source_code] String,
        #[label = "Here."] Span,
        Description,
        Help,
    ),
    #[error("Runtime error: {2}")]
    #[diagnostic(code(beemo::interpreter), help("{3}"))]
    RuntimeError(
        #[source_code] String,
        #[label = "Here."] Span,
        Description,
        Help,
    ),
    #[error("Internal error.")]
    InternalError,
}
