use thiserror::Error;

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

    #[error("at position {0}:{1}: Function call on non-identifier.")]
    InvalidFunctionCall(usize, usize),
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
}

// VM Runtime specific errors
#[derive(Error, Debug)]
pub enum VMRuntimeError {
    #[error("Attempted access to a register that does not exist. Tried to call {0}, but register lens is {1}. This is indicative of errornous bytecode, or a critical bug in the VM.")]
    AccessToNonExistentRegister(usize, usize),

    #[error("Attempted access to a constant that does not exist. Tried to get constant {0}, but constant pool lens is {1}. This is indicative of errornous bytecode, or a critical bug in the VM.")]
    AccessToNonExistentConstant(usize, usize),

    #[error("Attempted access to a string that does not exist. Tried to get string index {0}, but string pool lens is {1}. This is indicative of errornous bytecode, or a critical bug in the VM.")]
    AccessToNonExistentString(usize, usize),
}
