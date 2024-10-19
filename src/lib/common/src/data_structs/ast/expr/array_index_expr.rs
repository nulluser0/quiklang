//! # Array Index Expression AST Structures
//!
//! This module contains AST structures for array index expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Array Index Expression AST Structures
//! - [`ArrayIndexExpr`](#array-index-expr)

use crate::errors::Span;

use super::Expr;

/// Array index expression.
/// Represents an index into an array.
/// Example: `array[0]`, `array[1]`, `array[2]`, `hashmap["key"]`.
/// The index is zero-based.
/// The index must be a constant integer.
/// If the array is fixed-size, the index must be in bounds.
/// Otherwise, index out of bounds is a runtime error.
#[derive(Debug, Clone)]
pub struct ArrayIndexExpr {
    /// The array expression.
    pub array: Box<Expr>,
    /// The index into the array.
    pub index: Box<Expr>,
    /// Span
    pub span: Span,
}
