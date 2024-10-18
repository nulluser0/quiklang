//! # Bracket Expression AST Structures
//!
//! This module contains AST structures for bracket expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Bracket Expression AST Structures
//! - [`BracketExpr`](#bracket-expr)

use crate::Span;

use super::Expr;

/// Bracket expression.
/// Represents an expression enclosed in brackets.
/// Example: `(x + y) * z`.
/// This is used to change the order of operations.
/// The expression inside the brackets is evaluated first.
#[derive(Debug, Clone)]
pub struct BracketExpr {
    /// The expression inside the brackets.
    pub expr: Box<Expr>,
    /// Span
    pub span: Span,
}
