//! # Loop Expression AST Structures
//!
//! This module contains AST structures for loop expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Loop Expression AST Structures
//! - [`LoopExpr`](#loop-expr)

use quiklang_utils::Span;

use super::Expr;

/// Forever loop expression.
/// Represents a forever loop.
/// Example: `loop { println("Hello, World!") }`.
/// The loop block will be executed forever, until a `break` or `return` expression is encountered.
#[derive(Debug, Clone)]
pub struct LoopExpr {
    /// The block to execute forever.
    pub block: Box<Expr>,
    /// Span
    pub span: Span,
}
