use thiserror::Error;

use super::Span;

/// Lexer-Specific Errors
#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Unrecognized character '{character}' at line {}, column {}.", span.line, span.col)]
    UnrecognizedCharacter { character: char, span: Span },

    #[error("Unterminated string literal starting at line {}, column {}.", span.line, span.col)]
    UnterminatedStringLiteral { span: Span },

    #[error("Unterminated string interpolation starting at line {}, column {}.", span.line, span.col)]
    UnterminatedInterpolation { span: Span },

    #[error("Unterminated multi-line comment starting at line {}, column {}.", span.line, span.col)]
    UnterminatedMultiLineComment { span: Span },

    #[error("Invalid number format '{invalid_string}' at line {}, column {}.", span.line, span.col)]
    InvalidNumberFormat { invalid_string: String, span: Span },
}

impl LexerError {
    pub fn span(&self) -> Span {
        match self {
            LexerError::UnrecognizedCharacter { span, .. } => *span,
            LexerError::UnterminatedStringLiteral { span, .. } => *span,
            LexerError::UnterminatedInterpolation { span, .. } => *span,
            LexerError::UnterminatedMultiLineComment { span, .. } => *span,
            LexerError::InvalidNumberFormat { span, .. } => *span,
        }
    }
}

/// Lexer-Specific Warnings
#[derive(Debug, Error)]
pub enum LexerWarning {
    // Add lexer-specific warnings
}
