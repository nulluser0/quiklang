use thiserror::Error;

use crate::frontend::{ast::Expr, lexer::Token};

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Error at position {position}: {message}. Expected {expected:?}, but found {found:?}")]
    UnexpectedToken {
        expected: Token,
        found: Token,
        position: usize,
        message: String,
    },

    #[error("Error: `async` must be followed by `fn`")]
    MissingAsyncFn,

    #[error("Error at position {0}: `break` found outside of loop context")]
    BreakOutsideLoop(usize),

    #[error("Error at position {0}: `return` found outside of function context")]
    ReturnOutsideFunction(usize),

    #[error("Error at position {0}: Missing function identifier")]
    MissingFunctionIdentifier(usize),

    #[error("Error at position {0}: Invalid function parameter")]
    InvalidFunctionParameter(usize),

    #[error("Error at position {0}: Cannot use `mut` with `const`")]
    MutConstConflict(usize),

    #[error("Error at position {0}: Missing identifier")]
    MissingIdentifier(usize),

    #[error("Error at position {0}: `const` must have an assigned value")]
    ConstWithoutValue(usize),

    #[error("Error at position {0}: Object literal key expected")]
    ObjectLiteralKeyExpected(usize),

    #[error("Error at position {0}: Invalid operator {1:?}")]
    InvalidOperator(usize, Token),

    #[error("Error at position {0}: Invalid property access using dot operator {1:?}")]
    InvalidDotProperty(usize, Expr),
    // Add other error variants here if needed
}
