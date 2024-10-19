use thiserror::Error;

use crate::data_structs::tokens::TokenType;

use super::Span;

/// Parser-Specific Errors
#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Unexpected end of file at line {}, column {}.", span.line, span.col)]
    UnexpectedEOF { span: Span, suggestion: Vec<String> },
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
    // Add more parser-specific errors
}

impl ParserError {
    pub fn span(&self) -> Span {
        match self {
            ParserError::UnexpectedEOF { span, .. } => *span,
            ParserError::UnexpectedToken { span, .. } => *span,
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
        }
    }
}

/// Parser-Specific Warnings
#[derive(Debug, Error)]
pub enum ParserWarning {
    // Add parser-specific warnings
}
