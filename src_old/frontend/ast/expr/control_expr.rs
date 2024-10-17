//! # Control Expressions AST Structures
//!
//! This module contains AST structures for control expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Control Expression AST Structures
//! - [`ReturnExpr`](#return-expr)
//! - [`BreakExpr`](#break-expr)
//! - [`ContinueExpr`](#continue-expr)

use super::Expr;

/// Return expression.
/// Represents a return statement.
/// Example: `return 42`, `return "Hello, World!"`.
/// The return value must match the return type of the function.
/// The return value is optional.
/// If the return value is missing, the return type must be `null`.
/// The return value is the last expression in the block.
/// Compiler error if the return value mismatches the return type.
#[derive(Debug, Clone)]
pub struct ReturnExpr {
    /// The return value.
    pub value: Option<Box<Expr>>,
}

/// Break expression.
/// Represents a break statement.
/// Example: `break`, `break 42`, `break "Hello, World!"`.
/// The break value must match the loop type.
/// The break value is optional.
/// If the break value is missing, the loop type must be `null`.
/// Compiler error if the break value mismatches the loop type.
#[derive(Debug, Clone)]
pub struct BreakExpr {
    /// The break value.
    pub value: Option<Box<Expr>>,
}

/// Continue expression.
/// Represents a continue statement.
/// Example: `continue`.
/// The continue statement is used to skip the rest of the loop body and start the next iteration.
/// The continue statement is only allowed in loop bodies.
#[derive(Debug, Clone)]
pub struct ContinueExpr {}
