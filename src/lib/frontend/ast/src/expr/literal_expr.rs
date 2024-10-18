//! # Literal Expression AST Structures
//!
//! This module contains AST structures for literal expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Literal Expression AST Structures
//! - [`LiteralExpr`](#literal-expr)

use quiklang_utils::Span;

/// Literal expression.
/// Represents a literal value.
/// Example: `42`, `3.14`, `"Hello, World!"`.
/// List of literal types:
/// - `integer` Integer
/// - `float` Float
/// - `string` String
/// - `char` Char
/// - `bool` Boolean
/// - `null` Null
#[derive(Debug, Clone)]
pub struct LiteralExpr {
    /// The literal value.
    pub value: Literal,
    /// Span
    pub span: Span,
}

/// Literal.
/// Represents a literal value.
/// Example: `42`, `3.14`, `"Hello, World!"`.
#[derive(Debug, Clone)]
pub enum Literal {
    /// Integer literal.
    Integer(i64),
    /// Float literal.
    Float(f64),
    /// String literal.
    String(String),
    /// Char literal.
    Char(char),
    /// Boolean literal.
    Bool(bool),
    /// Null literal.
    Null,
}
