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

#[derive(Debug, Clone)]
pub struct File {
    pub id: usize,
    pub name: String,   // e.g., "src/main.quik"
    pub source: String, // The actual source code
}

/// Manages all files involved in the compilation process.
#[derive(Debug, Default)]
pub struct FileStore {
    files: HashMap<usize, File>,
    next_id: usize,
}

impl FileStore {
    /// Adds a new file to the store and returns its unique ID.
    pub fn add_file(&mut self, name: String, source: String) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        self.files.insert(id, File { id, name, source });
        id
    }

    /// Retrieves a file by its ID.
    pub fn get_file(&self, id: usize) -> Option<&File> {
        self.files.get(&id)
    }

    /// Retrieves a file by its name.
    pub fn get_file_by_name(&self, name: &str) -> Option<&File> {
        self.files.values().find(|f| f.name == name)
    }
}
