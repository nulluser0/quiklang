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

/// Represents a statement in Quiklang.
pub enum Stmt {
    /// An Expression statement. All Exprs are Stmts.
    Expr(Expr),
    /// Variable declaration statement.
    VarDecl(VarDeclStmt),
}

/// Represents a variable declaration statement.
pub struct VarDeclStmt {
    /// The name of the variable.
    pub name: &'static str,
    /// Is mutable?
    pub mutable: bool,
    /// The type of the variable.
    pub ty: Type,
    /// The value assigned to the variable.
    pub value: Expr,
}
