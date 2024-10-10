//! Identifier path expression AST Structures
//!
//! This module contains AST structures for identifier path expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Identifier Path Expression AST Structures
//! - [`IdentPathExpr`](#ident-path-expr)

/// Identifier path expression.
/// Represents a path to an identifier (or module).
/// Example: `std::io::println`.
#[derive(Debug, Clone)]
pub struct IdentPathExpr {
    /// The path to the module.
    pub path: Vec<SpecificPath>,
}

/// Specific path to an identifier (or module), representing one part of the path.
/// Example: `std` or `io` or `println`.
#[derive(Debug, Clone)]
pub struct SpecificPath {
    /// The name of the module.
    pub name: &'static str,
    /// Type arguments to the module.
    pub args: Vec<Type>,
}
