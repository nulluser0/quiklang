//! # Match Expression AST Structures
//!
//! This module contains AST structures for match expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Match Expression AST Structures
//! - [`MatchExpr`](#match-expr)
//! - [`MatchArm`](#match-arm)

use crate::errors::Span;

use super::Expr;

/// Match expression.
/// Represents a match expression.
/// Example: `match x { 0 => println("Zero"), 1 => println("One"), _ => println("Other") }`.
/// Useful for pattern matching and destructuring.
#[derive(Debug, Clone)]
pub struct MatchExpr {
    /// The expression to match.
    pub expr: Box<Expr>,
    /// The match arms.
    pub arms: Vec<MatchArm>,
    /// Span
    pub span: Span,
}

/// Match arm.
/// Represents a match arm in a match expression.
/// Example: `0 => println("Zero")`.
#[derive(Debug, Clone)]
pub struct MatchArm {
    /// The pattern to match.
    // pub pattern: MatchPattern,
    /// The expression to execute
    pub expr: Box<Expr>,
    /// Span
    pub span: Span,
}
