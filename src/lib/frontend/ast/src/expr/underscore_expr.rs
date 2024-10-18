//! # Underscore Expression AST Structure
//!
//! Represents an underscore expression in Quiklang.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Underscore Expression AST Structures
//! - [`UnderscoreExpr`](#underscore-expr)

use crate::Span;

/// Underscore expression.
/// Represents an underscore.
/// Example: `_`, `let _ = 42`.
/// The underscore is used to ignore a value.
/// The underscore is used to ignore a variable binding.
#[derive(Debug, Clone)]
pub struct UnderscoreExpr {
    /// Span
    pub span: Span,
}
