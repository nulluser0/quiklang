//! # Tuple Index Expression AST Structure
//!
//! Represents a tuple index expression in Quiklang.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Tuple Index Expression AST Structures
//! - [`TupleIndexExpr`](#tuple-index-expr)

use quiklang_utils::Span;

use super::Expr;

/// Tuple index expression.
/// Represents an index into a tuple.
/// Example: `tuple.0`, `tuple.1`, `tuple.2`.
/// The index is zero-based.
/// The index must be a constant integer.
/// The index must be in bounds.
/// The type of the expression is the type of the value at the index.
/// Compiler error if the index is out of bounds.
#[derive(Debug, Clone)]
pub struct TupleIndexExpr {
    /// The tuple expression.
    pub tuple: Box<Expr>,
    /// The index into the tuple.
    pub index: usize,
    /// Span
    pub span: Span,
}
