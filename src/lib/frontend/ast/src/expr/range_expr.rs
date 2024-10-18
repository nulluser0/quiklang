//! # Range Expression AST Structure
//!
//! Represents a range expression in Quiklang.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Range Expression AST Structures
//! - [`RangeExpr`](#range-expr)

use quiklang_utils::Span;

use super::Expr;

/// Range expression.
/// Represents a range.
/// Example: `0..10`, `0..=10`.
/// The range is inclusive on the start and exclusive on the end.
/// When the range is inclusive on the end, it is written as `..=`.
/// Implement the `IntoIterator` trait to use the range in a for loop.
#[derive(Debug, Clone)]
pub struct RangeExpr {
    /// The start of the range.
    pub start: Box<Expr>,
    /// The end of the range.
    pub end: Box<Expr>,
    /// Is the range inclusive on the end?
    pub inclusive: bool,
    /// Span
    pub span: Span,
}
