//! # Method Call Expression AST Structure
//!
//! Represents a method call expression in Quiklang.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Method Call Expression AST Structures
//! - [`MethodCallExpr`](#method-call-expr)

use crate::Span;

use super::Expr;

/// Method call expression.
/// Represents a method call.
/// Example: `string.len()`, `array.push(42)`.
/// The method is called on a struct/enum with a list of arguments.
/// The first argument is the struct/enum itself, automatically passed by dot syntax.
#[derive(Debug, Clone)]
pub struct MethodCallExpr {
    /// The method to call.
    pub method: Box<Expr>,
    /// The arguments to the method.
    pub args: Vec<Expr>,
    /// Span
    pub span: Span,
}
