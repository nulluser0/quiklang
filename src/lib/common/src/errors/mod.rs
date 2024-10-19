use thiserror::Error;

/// Centralized Compiler Error Enum
#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("{0}")]
    LexerError(#[from] LexerError),

    #[error("{0}")]
    ParserError(#[from] ParserError),
}

/// Lexer-Specific Errors
#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Unrecognized character '{character}' at line {}, column {}.", span.line, span.col)]
    UnrecognizedCharacter { character: char, span: Span },

    #[error("Unterminated string literal starting at line {}, column {}.", span.line, span.col)]
    UnterminatedStringLiteral { span: Span },

    #[error("Invalid number format '{invalid_string}' at line {}, column {}.", span.line, span.col)]
    InvalidNumberFormat { invalid_string: String, span: Span },

    #[error("Internal error. Please report this!: {0}")]
    InternalError(String),
}

/// Parser-Specific Errors
#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Unexpected end of file at line {}, column {}.", span.line, span.col)]
    UnexpectedEOF { span: Span },
    #[error(
        "Unexpected token '{found:?}' at line {}, column {}. Expected {expected}.",
        span.line,
        span.col
    )]
    UnexpectedToken {
        expected: String,
        found: TokenType,
        span: Span,
    },
    // Add more parser-specific errors
}

/// Warning Enum (similar to errors, if needed)
#[derive(Debug, Error)]
pub enum CompilerWarning {
    #[error("Unused variable '{name}' at line {}, column {}.", span.line, span.col)]
    UnusedVariable {
        name: String,
        span: Span,
        suggestion: Option<String>,
    },
    #[error("Deprecated feature '{feature}' used at line {}, column {}.", span.line, span.col)]
    DeprecatedFeature {
        feature: String,
        span: Span,
        suggestion: Option<String>,
    },
    // Add more warnings as needed
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

use std::collections::HashMap;

use crate::data_structs::tokens::TokenType;

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
