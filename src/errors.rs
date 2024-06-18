use thiserror::Error;

use crate::frontend::{ast::Expr, lexer::Token};

// Universal Errors used by functions like parser::produce_ast()
#[derive(Error, Debug)]
pub enum Error {
    #[error("Lexer Error: {0}")]
    LexerError(LexerError),

    #[error("Parser Error: {0}")]
    ParserError(ParserError),
    //
    // #[error("Type Error: {0}")]
    // TODO: TypeError(TypeError),

    // #[error("Runtime Error: {0}")]
    // TODO: RuntimeError(RuntimeError),
}

// Lexer-specific Errors
#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unrecognized character '{0}'")]
    UnrecognizedCharacter(char),

    #[error("Invalid number format: {0}")]
    InvalidNumberFormat(String),

    #[error("Unterminated string literal")]
    UnterminatedStringLiteral,

    #[error("Unexpected end of file")]
    UnexpectedEOF,

    #[error("Internal error: {0}")]
    InternalError(String),
}

// Parser-specific Errors
#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Lexer Error: {0}")]
    LexerErrorAsParseError(LexerError),

    #[error("Parser Error: at position {position}: {message}. Expected {expected:?}, but found {found:?}")]
    UnexpectedToken {
        expected: Token,
        found: Token,
        position: usize,
        message: String,
    },

    #[error("Parser Error: `async` must be followed by `fn`")]
    MissingAsyncFn,

    #[error("Parser Error: at position {0}: `break` found outside of loop context")]
    BreakOutsideLoop(usize),

    #[error("Parser Error: at position {0}: `return` found outside of function context")]
    ReturnOutsideFunction(usize),

    #[error("Parser Error: at position {0}: Missing function identifier")]
    MissingFunctionIdentifier(usize),

    #[error("Parser Error: at position {0}: Invalid function parameter")]
    InvalidFunctionParameter(usize),

    #[error("Parser Error: at position {0}: Cannot use `mut` with `const`")]
    MutConstConflict(usize),

    #[error("Parser Error: at position {0}: Missing identifier")]
    MissingIdentifier(usize),

    #[error("Parser Error: at position {0}: `const` must have an assigned value")]
    ConstWithoutValue(usize),

    #[error("Parser Error: at position {0}: Object literal key expected")]
    ObjectLiteralKeyExpected(usize),

    #[error("Parser Error: at position {0}: Invalid operator {1:?}")]
    InvalidOperator(usize, Token),

    #[error("Parser Error: at position {0}: Invalid property access using dot operator {1:?}")]
    InvalidDotProperty(usize, Expr),
    // Add other error variants here if needed
}
