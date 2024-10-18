//! # While Expression AST Structures
//!
//! This module contains AST structures for while expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of While Expression AST Structures
//! - [`WhileExpr`](#while-expr)

use quiklang_utils::Span;

use super::Expr;

/// While expression.
/// Represents a while loop.
/// Example: `while x < 10 { println(x as string); x += 1 }`.
/// The condition is an expression that evaluates to a boolean.
#[derive(Debug, Clone)]
pub struct WhileExpr {
    /// The condition to check.
    pub cond: Box<Expr>,
    /// The block to execute while the condition is true.
    pub block: Box<Expr>,
    /// Span
    pub span: Span,
}
