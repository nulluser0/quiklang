use thiserror::Error;

use super::Span;

/// Lexer-Specific Errors
#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Unrecognized character '{character}' at line {}, column {}.", span.line, span.col)]
    UnrecognizedCharacter {
        character: char,
        span: Span,
        suggestion: Vec<String>,
    },

    #[error("Unterminated string literal starting at line {}, column {}.", span.line, span.col)]
    UnterminatedStringLiteral {
        span: Span,
        suggestions: Vec<String>,
    },

    #[error("Unterminated string interpolation starting at line {}, column {}.", span.line, span.col)]
    UnterminatedInterpolation {
        span: Span,
        suggestions: Vec<String>,
    },

    #[error("Unterminated multi-line comment starting at line {}, column {}.", span.line, span.col)]
    UnterminatedMultiLineComment {
        span: Span,
        suggestions: Vec<String>,
    },

    #[error("Invalid number format '{invalid_string}' at line {}, column {}.", span.line, span.col)]
    InvalidNumberFormat {
        invalid_string: String,
        span: Span,
        suggestions: Vec<String>,
    },

    #[error("Invalid escape sequence at line {}, column {}.", span.line, span.col)]
    InvalidEscapeSequence {
        span: Span,
        suggestions: Vec<String>,
    },
}

impl LexerError {
    pub fn span(&self) -> Option<Span> {
        match self {
            LexerError::UnrecognizedCharacter { span, .. } => Some(*span),
            LexerError::UnterminatedStringLiteral { span, .. } => Some(*span),
            LexerError::UnterminatedInterpolation { span, .. } => Some(*span),
            LexerError::UnterminatedMultiLineComment { span, .. } => Some(*span),
            LexerError::InvalidNumberFormat { span, .. } => Some(*span),
            LexerError::InvalidEscapeSequence { span, .. } => Some(*span),
        }
    }

    pub fn suggestions(&self) -> Vec<String> {
        match self {
            LexerError::UnrecognizedCharacter { suggestion, .. } => suggestion.clone(),
            LexerError::UnterminatedStringLiteral {
                suggestions: suggestion,
                ..
            } => suggestion.clone(),
            LexerError::UnterminatedInterpolation {
                suggestions: suggestion,
                ..
            } => suggestion.clone(),
            LexerError::UnterminatedMultiLineComment {
                suggestions: suggestion,
                ..
            } => suggestion.clone(),
            LexerError::InvalidNumberFormat {
                suggestions: suggestion,
                ..
            } => suggestion.clone(),
            LexerError::InvalidEscapeSequence {
                suggestions: suggestion,
                ..
            } => suggestion.clone(),
        }
    }
}

/// Lexer-Specific Warnings
#[derive(Debug, Error)]
pub enum LexerWarning {
    // Add lexer-specific warnings
}
