//! # Tuple Expression AST Structures
//!
//! This module contains AST structures for tuple expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Tuple Expression AST Structures
//! - [`TupleExpr`](#tuple-expr)

use super::Expr;

/// Tuple expression.
/// Represents a tuple.
/// Example: `(42, 3.14, "Hello, World!")`.
/// A tuple is a fixed-size collection of values.
/// The values can be of different types.
#[derive(Debug, Clone)]
pub struct TupleExpr {
    /// The values in the tuple.
    pub values: Vec<Expr>,
}
