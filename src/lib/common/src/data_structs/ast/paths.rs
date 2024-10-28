//! # Paths module

use crate::errors::Span;

use super::types::ASTType;

/// This module contains the AST structures for paths in Quiklang.
#[derive(Debug, Clone)]
pub struct ASTPath {
    /// The path segments in the path.
    pub segments: Vec<PathSegment>,
    /// Span
    pub span: Span,
}

/// A segment in a path.
#[derive(Debug, Clone)]
pub struct PathSegment {
    /// The name of the segment.
    pub name: String,
    /// ASTType arguments to the module.
    pub args: Vec<ASTType>,
    /// Span
    pub span: Span,
}
