//! Enum Expression AST Structures
//!
//! This module contains AST structures for enum expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Enum Expression AST Structures
//! - [`EnumExpr`](#enum-expr)

use super::{struct_expr::FilledStructField, Expr};

/// Enum expression.
/// Represents an instance of an enum variant being created.
/// Example: `Variant::Value`, `Variant::Tuple(42, 3.14)`, `Variant::Struct { x: 1, y: 2 }`.
#[derive(Debug, Clone)]
pub struct EnumExpr {
    /// The name of the enum.
    pub name: &'static str,
    /// The variant of the enum.
    pub variant: FilledEnumVariant,
}

/// Filled enum variant.
/// Represents a variant in an enum that is filled with a value.
#[derive(Debug, Clone)]
pub struct FilledEnumVariant {
    /// The name of the variant.
    pub name: &'static str,
    /// The value of the variant.
    pub value: Option<FilledEnumField>,
}

/// Filled enum field.
/// Represents a field in an enum variant that is filled with a value.
#[derive(Debug, Clone)]
pub enum FilledEnumField {
    /// No fields.
    None,
    /// Tuple fields.
    Tuple(Vec<Expr>),
    /// Struct fields.
    Struct(Vec<FilledStructField>),
}
