//! # Field Access Expression AST Structure
//!
//! Represents a field access expression in Quiklang.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Field Access Expression AST Structures
//! - [`FieldAccessExpr`](#field-access-expr)

use super::Expr;

/// Field access expression.
/// Represents an access to a field in a struct or enum.
/// Example: `point.x`, `point.y`, `variant.field`.
/// The field must exist in the struct or enum.
/// The type of the expression is the type of the field.
/// Compiler error if the field does not exist.
/// Compiler error if the field is private.
#[derive(Debug, Clone)]
pub struct FieldAccessExpr {
    /// The struct/enum expression.
    pub expr: Box<Expr>,
    /// The name of the field.
    pub field: &'static str,
}
