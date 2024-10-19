pub mod lexer;
pub mod parser;

use std::collections::HashMap;

use lexer::{LexerError, LexerWarning};
use parser::{ParserError, ParserWarning};
use thiserror::Error;

/// Centralized Compiler Error Enum
#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("{0}")]
    LexerError(#[from] LexerError),

    #[error("{0}")]
    ParserError(#[from] ParserError),
}

/// Centralized Compiler Warning Enum
#[derive(Debug, Error)]
pub enum CompilerWarning {
    #[error("{0}")]
    LexerWarning(#[from] LexerWarning),

    #[error("{0}")]
    ParserWarning(#[from] ParserWarning),
}

/// Span Struct to Represent Error Locations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub file_id: usize, // Reference to FileStore's file ID
    pub start: usize,   // Byte offset from the start of the file
    pub end: usize,     // Byte offset where the span ends
    pub line: usize,    // Line number (1-based indexing)
    pub col: usize,     // Column number (1-based indexing)
}

impl Span {
    pub fn new(file_id: usize, start: usize, end: usize, line: usize, col: usize) -> Self {
        Span {
            file_id,
            start,
            end,
            line,
            col,
        }
    }
}
