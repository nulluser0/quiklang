//! This module contains the error types used in the application.
//!
//! The error types are organized into different enums based on their usage:
//! - `Error`: Universal errors used by functions like `produce_ast()`.
//! - `LexerError`: Lexer-specific errors.
//! - `ParserError`: Parser-specific errors.
//! - `InterpreterError`: Interpreter-specific errors.
//! - `VMCompileError`: VM compile-specific errors.
//! - `VMBytecodeError`: VM bytecode-specific errors.
//! - `VMRuntimeError`: VM runtime-specific errors.
//!
//! Each error enum provides detailed error messages for different scenarios.
//! The error messages include information such as the position of the error,
//! expected and found values, and specific error descriptions.
//!
//! The error types are implemented using the `thiserror` crate, which allows
//! for easy generation of error types with custom error messages.
//!
//! Example usage:
//!
//! ```rust
//! use crate::errors::{Error, LexerError};
//!
//! fn tokenize(input: &str) -> Result<Vec<Token>, Error> {
//!     // Tokenize the input
//!     // ...
//!     // If an error occurs during tokenization, return a LexerError
//!     Err(Error::LexerError(LexerError::UnrecognizedCharacter {
//!         character: '!',
//!         line: 1,
//!         col: 5,
//!     }))
//! }
//! ```

use thiserror::Error;
use tokio::task::JoinError;

use crate::{
    backend_interpreter::values::ValueType,
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

    // Interpreter Errors:
    #[error("Interpreter Error: {0}")]
    InterpreterError(#[from] InterpreterError),

    // VM Errors:
    #[error("VM Compile Error: {0}")]
    VMCompileError(#[from] VMCompileError),

    #[error("VM Bytecode Error: {0}")]
    VMBytecodeError(#[from] VMBytecodeError),

    #[error("VM Runtime Error: {0}")]
    VMRuntimeError(#[from] VMRuntimeError),
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
    #[error("at position {line}:{col}: {message}. Expected {expected}, but found {found}")]
    UnexpectedToken {
        expected: TokenType,
        found: TokenType,
        line: usize,
        col: usize,
        message: String,
    },

    #[error("`async` must be followed by `fn`")]
    MissingAsyncFn,

    #[error("`extern` must be followed by `fn`")]
    MissingExternFn,

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

    #[error("at position {0}:{1}: Invalid operator {1}")]
    InvalidOperator(usize, usize, TokenType),

    #[error("at position {0}:{1}: Invalid property access using dot operator {1}")]
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

    #[error(
        "at position {line}:{col}: Mutability mismatch. Expected is_mutable = {expected}, but found is_mutable = {found}"
    )]
    MutabilityMismatch {
        expected: bool,
        found: bool,
        line: usize,
        col: usize,
    },

    #[error("at position {0}:{1}: Cannot declare already existing variable '{2}'")]
    DeclaredExistingVariable(usize, usize, String),

    #[error("at position {0}:{1}: Cannot declare already existing function '{2}'")]
    DeclaredExistingFunction(usize, usize, String),

    #[error("at position {0}:{1}: Cannot declare already existing struct '{2}'")]
    DeclaredExistingStruct(usize, usize, String),

    #[error("at position {0}:{1}: Cannot declare already existing enum '{2}'")]
    DeclaredExistingEnum(usize, usize, String),

    #[error("at position {0}:{1}: Cannot declare already existing type alias '{2}'")]
    DeclaredExistingAlias(usize, usize, String),

    #[error("at position {0}:{1}: Cannot resolve non-existent variable '{2}'")]
    UndefinedVariable(usize, usize, String),

    #[error("at position {0}:{1}: Cannot resolve non-existent function '{2}'")]
    UndefinedFunction(usize, usize, String),

    #[error("at position {0}:{1}: Function call on non-identifier")]
    FunctionCallOnNonIdent(usize, usize),

    #[error("at position {0}:{1}: Function call on non-function identifier '{2}'")]
    FunctionCallOnNonFunction(usize, usize, String),
}

// Interpreter-specific Errors
#[derive(Error, Debug)]
pub enum InterpreterError {
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

// VM Compile specific errors
#[derive(Error, Debug)]
pub enum VMCompileError {
    #[error("IO Error occurred: {0}")]
    IOError(#[from] std::io::Error),

    #[error("Cannot resolve non-existent symbol '{0}'")]
    UndefinedVariable(String),

    #[error("Cannot resolve type from typedef. {0}")]
    UndefinedType(String),

    #[error("Unexpected type mismatch.")]
    UnexpectedMismatch,
}

// VM Bytecode specific errors
#[derive(Error, Debug)]
pub enum VMBytecodeError {
    #[error("IO Error occurred: {0}")]
    IOError(#[from] std::io::Error),

    #[error("String parse error: {0}")]
    StringParseError(#[from] std::string::FromUtf8Error),

    #[error("Invalid constant type: {0}")]
    InvalidConstantType(u8),

    #[error("Bytecode being read contains invalid Magic Number.")]
    InvalidOrNoMagicNumber,

    #[error("Bytecode is inconpatible with the current VM version. VM requires version: {0}, but bytecode is version: {1}. Please recompile the source code.")]
    IncompatibleBytecodeVersion(i32, i32),
}

// VM Runtime specific errors
#[derive(Error, Debug)]
#[repr(C)]
pub enum VMRuntimeError {
    #[error("STACK OVERFLOW")]
    StackOverflow,

    #[error("STACK UNDERFLOW")]
    StackUnderflow,

    #[error("Invalid Opcode '{0} ({0:#x})' at program counter {1}. Indicative of: \nVM bug, \nCorrupted Bytecode, \nInvalid Bytecode, \nInvalid VM state, \nIncompatible bytecode. \nTry recompiling the source code, or report this issue.")]
    InvalidOpcode(i32, usize),

    #[error("Native Function not found at index {0}.")]
    UndefinedNativeFn(usize),

    #[error("QFFI Function not found at index {0}.")]
    UndefinedQFFIFn(usize),

    #[error("QFFI/Native Function Error: {0}")]
    QFFINativeFnError(String),

    #[error("Access to a register that does not exist: {0}")]
    InvalidRegisterAccess(usize),

    #[error("Access to a constant that does not exist: {0}")]
    InvalidConstantAccess(usize),

    #[error("Failed to join thread: {0}. JoinError: {1}")]
    ThreadJoinError(usize, JoinError),

    #[error("Invalid thread ID: {0}")]
    InvalidThreadId(usize),

    #[error("Division by zero.")]
    DivideByZero,

    #[error("Null Pointer Dereference.")]
    NullPtrDeref, // Pointer represented as 8 bytes, Register represented as 4 bytes from instruction

    #[error("Panic: {0}")]
    Panic(String),

    #[error("{0}")]
    Exit(i32),
}
