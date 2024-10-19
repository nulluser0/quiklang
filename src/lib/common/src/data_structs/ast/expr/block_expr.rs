//! # Block Expression AST Structures
//!
//! This module contains AST structures for block expressions.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Block Expression AST Structures
//! - [`BlockExpr`](#block-expr)

use crate::{data_structs::ast::stmt::Stmt, errors::Span};

/// Block expression.
/// Represents a block of statements.
/// Example: `{ let x = 42; let y = 3.14; x + y }`.
/// The last expression in the block is the return value.
/// The block can have local variables, which will be dropped when the block ends.
#[derive(Debug, Clone)]
pub struct BlockExpr {
    /// The statements in the block.
    pub stmts: Vec<Stmt>,
    /// Span
    pub span: Span,
}
