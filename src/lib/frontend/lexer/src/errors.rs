use thiserror::Error;

// Lexer-specific Errors
#[derive(Error, Debug)]
pub enum LexerError {
    #[error("at position {line}:{col}: Unrecognized character '{character}'")]
    UnrecognizedCharacter {
        character: char,
        line: usize,
        col: usize,
    },

    #[error("at position {line}:{col}: Invalid number format: {invalid_string}")]
    InvalidNumberFormat {
        invalid_string: String,
        line: usize,
        col: usize,
    },

    #[error("at position {line}:{col}: Unterminated string literal at string beginning")]
    UnterminatedStringLiteral { line: usize, col: usize },

    #[error("at position {line}:{col}: Unexpected end of file")]
    UnexpectedEOF { line: usize, col: usize },

    #[error("errror during string interpolation: {0}")]
    StringInterpolationError(Box<LexerError>),

    #[error("Internal error. Please report this!: {0}")]
    InternalError(String),
}
