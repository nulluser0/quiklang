//! # Paths module

use crate::errors::Span;

use super::types::ASTType;

/// Simple Path.
#[derive(Debug, Clone)]
pub struct ASTSimplePath {
    /// The path segments in the path.
    pub segments: Vec<SimplePathSegment>,
    /// Span
    pub span: Span,
}

/// A segment in a Simple Path.
#[derive(Debug, Clone)]
pub struct SimplePathSegment {
    /// The name of the segment.
    pub name: String,
    /// Span
    pub span: Span,
}

/// Expr Paths.
#[derive(Debug, Clone)]
pub struct ASTExprPath {
    /// The path segments in the path.
    pub segments: Vec<ExprPathSegment>,
    /// Span
    pub span: Span,
}

/// A segment in an Expr Path.
#[derive(Debug, Clone)]
pub struct ExprPathSegment {
    /// The name of the segment.
    pub name: String,
    /// ASTType arguments to the module.
    pub args: Vec<ASTType>,
    /// Span
    pub span: Span,
}

/// Type Paths.
#[derive(Debug, Clone)]
pub struct ASTTypePath {
    /// The path segments in the path.
    pub segments: Vec<TypePathSegment>,
    /// Span
    pub span: Span,
}

/// A segment in a Type Path.
#[derive(Debug, Clone)]
pub struct TypePathSegment {
    /// The name of the segment.
    pub name: String,
    /// ASTType arguments to the module.
    pub args: Vec<ASTType>,
    /// Span
    pub span: Span,
}
