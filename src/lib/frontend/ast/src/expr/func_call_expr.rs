//! # Function Call Expression AST Structures
//!
//! This module contains AST structures for function call expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Function Call Expression AST Structures
//! - [`FuncCallExpr`](#func-call-expr)

use quiklang_utils::Span;

use super::Expr;

/// Function call expression.
/// Represents a function call.
/// Example: `println("Hello, World!")`.
/// The function is called with a list of arguments.
/// The arguments are expressions.
#[derive(Debug, Clone)]
pub struct FuncCallExpr {
    /// The function to call.
    pub func: Box<Expr>,
    /// The arguments to the function.
    pub args: Vec<Expr>,
    /// Span
    pub span: Span,
}
