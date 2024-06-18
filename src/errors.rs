use thiserror::Error;

use crate::{
    backend::values::ValueType,
    frontend::{
        ast::{BinaryOp, Expr},
        lexer::Token,
    },
};

// Universal Errors used by functions like parser::produce_ast()
#[derive(Error, Debug)]
pub enum Error {
    #[error("Lexer Error: {0}")]
    LexerError(#[from] LexerError),

    #[error("Parser Error: {0}")]
    ParserError(#[from] ParserError),
    //
    // #[error("Type Error: {0}")]
    // TODO: TypeError(#[from] TypeError),
    //
    #[error("Runtime Error: {0}")]
    RuntimeError(#[from] RuntimeError),
}

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

    #[error("Internal error. Please report this!: {0}")]
    InternalError(String),
}

// Parser-specific Errors
#[derive(Error, Debug)]
pub enum ParserError {
    #[error("at position {position}: {message}. Expected {expected:?}, but found {found:?}")]
    UnexpectedToken {
        expected: Token,
        found: Token,
        position: usize,
        message: String,
    },

    #[error("`async` must be followed by `fn`")]
    MissingAsyncFn,

    #[error("at position {0}: `break` found outside of loop context")]
    BreakOutsideLoop(usize),

    #[error("at position {0}: `return` found outside of function context")]
    ReturnOutsideFunction(usize),

    #[error("at position {0}: Missing function identifier")]
    MissingFunctionIdentifier(usize),

    #[error("at position {0}: Invalid function parameter")]
    InvalidFunctionParameter(usize),

    #[error("at position {0}: Cannot use `mut` with `const`")]
    MutConstConflict(usize),

    #[error("at position {0}: Missing identifier")]
    MissingIdentifier(usize),

    #[error("at position {0}: `const` must have an assigned value")]
    ConstWithoutValue(usize),

    #[error("at position {0}: Object literal key expected")]
    ObjectLiteralKeyExpected(usize),

    #[error("at position {0}: Invalid operator {1:?}")]
    InvalidOperator(usize, Token),

    #[error("at position {0}: Invalid property access using dot operator {1:?}")]
    InvalidDotProperty(usize, Expr),
    // Add other error variants here if needed
}

// Interpreter-specific Errors
#[derive(Error, Debug)]
pub enum RuntimeError {
    // Variables
    #[error("Cannot resolve non-existent variable '{0}'")]
    UndefinedVariable(String),

    #[error("Cannot declare already existing variable '{0}'")]
    DeclaredExistingVariable(String),

    #[error("Cannot assign to immutable variable '{0}'")]
    ImmutableVariableEdit(String),

    #[error("Type mismatch: {message} - expected {expected}, found {found}")]
    TypeError {
        message: String,
        expected: ValueType,
        found: ValueType,
    },

    #[error("Invalid assignee for assignment expression: '{0}'. Did you mean to use '==' instead of '='?")]
    InvalidAssignExpr(String),

    #[error("Unsupported Binary Operation: {0}")]
    UnsupportedBinaryOp(BinaryOp),

    // #[error("Index out of bounds: {0}")]
    // IndexOutOfBounds(usize),

    // #[error("Property '{0}' not found")]
    // PropertyNotFound(String),
    //
    #[error("Other runtime error: {0}")]
    RuntimeError(String),
}
