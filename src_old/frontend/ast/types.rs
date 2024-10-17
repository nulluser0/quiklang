//! # Types AST Structures
//!
//! This module contains the AST structures for types in Quiklang.
//!
//! [`Return to AST Module`](../index.html)
//!
//! ## List of Type AST Structures
//! - [`ASTType`](#ast-type)

use super::{expr::identpath_expr::ASTPath, package_module::TraitBound};

/// Way of representing types in the Quiklang AST.
#[derive(Debug, Clone)]
pub struct ASTType {
    pub kind: ASTTypeKind,
}

/// The kind of type in the Quiklang AST.
/// Represents the different types of types in Quiklang.
#[derive(Debug, Clone)]
pub enum ASTTypeKind {
    /// A path to a type.
    Path(ASTPath),
    /// A tuple type.
    Tuple(Vec<ASTType>),
    /// A ref type.
    Ref(Box<ASTType>, bool), // Type, mutable
    /// An array type.
    Array(Box<ASTType>),
    /// A function type.
    Fn(Vec<ASTType>, Box<ASTType>),
    /// Trait object type.
    TraitObject(Vec<ASTPath>),
    /// Type alias type.
    Alias(ASTPath),
    /// Placeholder type.
    ImplTrait,
    /// Self type.
    SelfType,
    /// Never type.
    Never,
    /// Null type.
    Null,
    /// Infer type.
    Infer,
    /// Cannot identify type here.
    Unknown,
}

/// Generic type parameter.
/// Represents a generic type parameter in a type.
/// Example: `T` in `Option<T>`.
#[derive(Debug, Clone)]
pub struct ASTGeneric {
    /// The bounds of the generic type parameter.
    pub bounds: Vec<TraitBound>,
}
