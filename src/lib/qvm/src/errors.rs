use thiserror::Error;
use tokio::task::JoinError;

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
