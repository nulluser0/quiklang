//! # Struct Expression AST Structures
//!
//! This module contains AST structures for struct expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Struct Expression AST Structures
//! - [`StructExpr`](#struct-expr)

use super::Expr;

/// Struct expression.
/// Represents an instance of a struct being created.
/// Example: `Point { x: 1, y: 2 }`.
#[derive(Debug, Clone)]
pub struct StructExpr {
    /// The name of the struct.
    pub name: &'static str,
    /// The fields of the struct.
    pub fields: Vec<FilledStructField>,
}

/// Filled struct field.
/// Represents a field in a struct that is filled with a value.
#[derive(Debug, Clone)]
pub struct FilledStructField {
    /// The name of the field.
    pub name: &'static str,
    /// The value of the field.
    pub value: Expr,
}
