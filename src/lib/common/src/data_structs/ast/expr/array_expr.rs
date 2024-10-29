//! # Array Expression AST Structures
//!
//! This module contains AST structures for array expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Array Expression AST Structures
//! - [`ArrayExpr`](#array-expr)

use crate::errors::Span;

use super::Expr;

/// Array expression.
/// Represents a fixed size array.
/// Example: `[1, 2, 3, 4, 5]`. '[T; N]'.
/// Arrays are fixed sized collections of values.
/// For dynamic sized collections, use a list.
#[derive(Debug, Clone)]
pub struct ArrayExpr {
    /// The values in the array.
    pub values: Vec<Expr>,
    /// Span
    pub span: Span,
}

/// List Array Expression.
/// Represents a dynamic sized array.
/// Example: `list[1, 2, 3, 4, 5]`. 'list[T]'
/// Lists are dynamic sized collections of values.
/// For fixed sized collections, use an array.
#[derive(Debug, Clone)]
pub struct ListArrayExpr {
    /// The values in the list.
    pub values: Vec<Expr>,
    /// Span
    pub span: Span,
}
