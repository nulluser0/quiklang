use thiserror::Error;

use crate::{
    backend::values::ValueType,
    frontend::{
        ast::{BinaryOp, Expr, Type},
        lexer::TokenType,
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
    #[error("at position {line}:{col}: {message}. Expected {expected:?}, but found {found:?}")]
    UnexpectedToken {
        expected: TokenType,
        found: TokenType,
        line: usize,
        col: usize,
        message: String,
    },

    #[error("`async` must be followed by `fn`")]
    MissingAsyncFn,

    #[error("at position {0}:{1}: `break` found outside of loop context")]
    BreakOutsideLoop(usize, usize),

    #[error("at position {0}:{1}: `return` found outside of function context")]
    ReturnOutsideFunction(usize, usize),

    #[error("at position {0}:{1}: Missing function identifier")]
    MissingFunctionIdentifier(usize, usize),

    #[error("at position {0}:{1}: Missing type for variable declaration")]
    MissingTypeForVarDeclaration(usize, usize),

    #[error("at position {0}:{1}: Invalid function parameter")]
    InvalidFunctionParameter(usize, usize),

    #[error("at position {0}:{1}: Cannot use `mut` with `const`")]
    MutConstConflict(usize, usize),

    #[error("at position {0}:{1}: Missing identifier")]
    MissingIdentifier(usize, usize),

    #[error("at position {0}:{1}: `const` must have an assigned value")]
    ConstWithoutValue(usize, usize),

    #[error("at position {0}:{1}: Object literal key expected")]
    ObjectLiteralKeyExpected(usize, usize),

    #[error("at position {0}:{1}: Invalid operator {1:?}")]
    InvalidOperator(usize, usize, TokenType),

    #[error("at position {0}:{1}: Invalid property access using dot operator {1:?}")]
    InvalidDotProperty(usize, usize, Expr),

    #[error("at position {0}:{1}: Invalid type declaration")]
    InvalidTypeDeclaration(usize, usize),

    #[error("at position {0}:{1}: Multiple return types.")]
    MultipleReturnTypes(usize, usize),

    #[error(
        "at position {line}:{col}: Type error: {message}. Expected {expected}, but found {found}"
    )]
    TypeError {
        expected: Type,
        found: Type,
        line: usize,
        col: usize,
        message: String,
    },
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
