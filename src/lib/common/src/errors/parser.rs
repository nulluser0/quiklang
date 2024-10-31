use thiserror::Error;

use crate::data_structs::tokens::TokenType;

use super::Span;

/// Parser-Specific Errors
#[derive(Debug, Error)]
pub enum ParserError {
    #[error("File ID {file_id} not found in the file store.")]
    FileNotFound { file_id: usize },

    #[error("No valid entry point found in the package.")]
    NoEntryPoint { suggestion: Vec<String> },

    #[error("Unexpected end of file at line {}, column {}.", span.line, span.col)]
    UnexpectedEOF { span: Span, suggestion: Vec<String> },

    #[error("Module not found: {module_name}.")]
    ModuleNotFound {
        module_name: String,
        span: Span,
        suggestion: Vec<String>,
    },

    #[error(
        "Unexpected token '{found:?}' at line {}, column {}. Expected {expected}.",
        span.line,
        span.col
    )]
    UnexpectedToken {
        expected: String,
        found: TokenType,
        span: Span,
        suggestion: Vec<String>,
    },

    #[error("Missing semicolon at line {}, column {}.", span.line, span.col)]
    MissingSemicolon { span: Span, suggestion: Vec<String> },
    // Add more parser-specific errors
}

impl ParserError {
    pub fn span(&self) -> Option<Span> {
        match self {
            ParserError::UnexpectedEOF { span, .. } => Some(*span),
            ParserError::UnexpectedToken { span, .. } => Some(*span),
            ParserError::FileNotFound { .. } => None,
            ParserError::NoEntryPoint { .. } => None,
            ParserError::ModuleNotFound { span, .. } => Some(*span),
            ParserError::MissingSemicolon { span, .. } => Some(*span),
        }
    }

    pub fn suggestions(&self) -> Vec<String> {
        match self {
            ParserError::UnexpectedEOF {
                suggestion: suggestions,
                ..
            } => suggestions.clone(),
            ParserError::UnexpectedToken {
                suggestion: suggestions,
                ..
            } => suggestions.clone(),
            ParserError::FileNotFound { .. } => vec![],
            ParserError::NoEntryPoint { suggestion } => suggestion.clone(),
            ParserError::ModuleNotFound {
                suggestion: suggestions,
                ..
            } => suggestions.clone(),
            ParserError::MissingSemicolon {
                suggestion: suggestions,
                ..
            } => suggestions.clone(),
        }
    }
}

/// Parser-Specific Warnings
#[derive(Debug, Error)]
pub enum ParserWarning {
    // Add parser-specific warnings
}
