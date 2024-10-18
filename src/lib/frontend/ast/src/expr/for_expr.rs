//! # For Expression AST Structures
//!
//! This module contains AST structures for for expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of For Expression AST Structures
//! - [`ForExpr`](#for-expr)

use quiklang_utils::Span;

use super::Expr;

/// For expression.
/// Represents a for loop.
/// Example: `for x in 0..10 { println(x) }`, `for x in [1, 2, 3] { println(x as string) }`.
/// The for loop iterable needs to implement the `IntoIterator` trait.
#[derive(Debug, Clone)]
pub struct ForExpr {
    /// The variable name.
    pub name: &'static str,
    /// The iterable to loop over.
    pub iterable: Box<Expr>,
    /// The block to execute for each item in the iterable.
    pub block: Box<Expr>,
    /// Span
    pub span: Span,
}
