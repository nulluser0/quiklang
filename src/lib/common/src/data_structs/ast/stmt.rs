//! # Statements AST Structures
//!
//! This module contains the AST structures for statements in Quiklang.
//!
//! [`Return to AST Module`](../index.html)
//!
//! ## List of Statement AST Structures
//!
//! | Structure  | Description |
//! |------------|-------------|
//! | Expr       | Represents an expression statement. All Expressions are Statements. |
//! | VarDecl    | Represents a variable declaration statement. |

use crate::errors::Span;

use super::{expr::Expr, types::ASTTypeKind};

/// Represents a statement in Quiklang.
#[derive(Debug, Clone)]
pub enum Stmt {
    /// An Expression statement. All Exprs are Stmts.
    Expr(Expr),
    /// Variable declaration statement.
    VarDecl(VarDeclStmt),
}

/// Represents a variable declaration statement.
#[derive(Debug, Clone)]
pub struct VarDeclStmt {
    /// The name of the variable.
    pub name: String,
    /// Is mutable?
    pub mutable: bool,
    /// The type of the variable. Optional, as it can be inferred.
    pub ty: ASTTypeKind,
    /// The value assigned to the variable. Can be uninitialized.
    pub value: Option<Expr>,
    /// Span
    pub span: Span,
}
