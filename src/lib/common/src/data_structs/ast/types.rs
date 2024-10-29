//! # Types AST Structures
//!
//! This module contains the AST structures for types in Quiklang.
//!
//! [`Return to AST Module`](../index.html)
//!
//! ## List of Type AST Structures
//! - [`ASTType`](#ast-type)

use super::{package_module::TraitBound, paths::ASTPath};

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
    /// A primitive type.
    Primitive(PrimitiveType),
    /// A ref type.
    Ref(Box<ASTType>, bool), // Type, mutable
    /// Trait object type.
    TraitObject(Vec<ASTPath>),
    /// Type alias type.
    Alias(ASTPath),
    /// Placeholder type.
    ImplTrait,
    /// Self type. The type which represents the type of `self` in a trait or impl.
    SelfType,
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

/// Primitive types in Quiklang.
/// Compiler-built-in types.
#[derive(Debug, Clone)]
pub enum PrimitiveType {
    /// A boolean type. 'bool'
    Bool,
    /// An integer type. 'integer'
    Integer,
    /// A floating-point type. 'float'
    Float,
    /// A character type. 'char'
    Char,
    /// A string type. 'str'
    String,
    /// A null type. 'null'
    Null,
    /// A void type. 'void'
    Void,
    /// A never type. '!'
    Never,
    /// A tuple type. '(T1, T2, T3)'
    Tuple(Vec<ASTType>),
    /// A fixed-size array type. '[T; N]'
    Array(Box<ASTType>, usize),
    /// A dynamic-size array type. 'list[T]'
    ListArray(Box<ASTType>),
    /// A function type. '(param1, param2) -> return'
    Fn(Vec<ASTType>, Box<ASTType>),
}
