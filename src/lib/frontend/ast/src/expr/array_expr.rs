//! # Array Expression AST Structures
//!
//! This module contains AST structures for array expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Array Expression AST Structures
//! - [`ArrayExpr`](#array-expr)

use quiklang_utils::Span;

use super::Expr;

/// Array expression.
/// Represents an array.
/// Example: `[1, 2, 3, 4, 5]`.
/// Arrays are dynamically sized collections of values.
/// The compiler however can infer if the array is a fixed-size array.
#[derive(Debug, Clone)]
pub struct ArrayExpr {
    /// The values in the array.
    pub values: Vec<Expr>,
    /// The size of the array.
    pub size: Option<usize>,
    /// Span
    pub span: Span,
}
