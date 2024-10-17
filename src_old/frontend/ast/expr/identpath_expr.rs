//! Identifier path expression AST Structures
//!
//! This module contains AST structures for identifier path expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Identifier Path Expression AST Structures
//! - [`IdentPathExpr`](#ident-path-expr)

use crate::frontend::ast::types::ASTTypeKind;

/// Path types
#[derive(Debug, Clone)]
pub enum ASTPath {
    /// Absolute path. Example: `std::io::println`.
    Absolute(AbsoluteASTPath),
    /// Relative (to type) path. Example: `<T as trait>::Item`. Typed.
    Relative(RelativeASTPath),
}

/// Identifier path expression.
/// Represents a path to an identifier (or module).
/// Example: `std::io::println`.
#[derive(Debug, Clone)]
pub struct AbsoluteASTPath {
    /// The path to the module.
    pub path: Vec<SpecificPath>,
}

/// Specific path to an identifier (or module), representing one part of the path.
/// Example: `std` or `io` or `println`.
#[derive(Debug, Clone)]
pub struct SpecificPath {
    /// The name of the module.
    pub name: &'static str,
    /// The ID of the module.
    pub id: usize,
    /// ASTTypeKind arguments to the module.
    pub args: Vec<ASTTypeKind>,
}

/// Relative path to an identifier (or module), representing one part of the path.
/// Example: `<T as trait>::Item`.
/// Typed.
#[derive(Debug, Clone)]
pub struct RelativeASTPath {
    /// The type of the path.
    pub ty: Box<ASTTypeKind>,
    /// The (only) path to the module.
    pub path: SpecificPath,
}
