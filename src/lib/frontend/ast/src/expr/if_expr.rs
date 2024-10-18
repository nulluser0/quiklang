//! # If Expression AST Structures
//!
//! This module contains AST structures for if expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of If Expression AST Structures
//! - [`IfExpr`](#if-expr)

use super::Expr;

/// If expression.
/// Represents an if expression.
/// Example: `if x < y { println("x is less than y") } else { println("x is greater than or equal to y") }`.
/// The condition is an expression that evaluates to a boolean.
/// The then block is executed if the condition is true.
/// The else block is executed if the condition is false.
/// The else block is optional.
/// The if expression returns a value if both the then and else blocks return a value. Else, it returns `null`.
/// Quiklang will complain if it else block is missing when a value is returned from the then block.
#[derive(Debug, Clone)]
pub struct IfExpr {
    /// The condition to check.
    pub cond: Box<Expr>,
    /// The then block to execute if the condition is true.
    pub then_block: Box<Expr>,
    /// The else block to execute if the condition is false.
    pub else_block: Option<Box<Expr>>,
}
