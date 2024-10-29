//! # Package and Module AST Structures
//!
//! The `package_module` module contains the AST structures for packages and modules in Quiklang.
//!
//! [`Return to AST Module`](../index.html)
//!
//! ## List of Package and Module AST Structures
//!
//! | Structure  | Description |
//! |------------|-------------|
//! | Package    | Represents a package in Quiklang. A package is a collection of modules, dependencies, and other items. It is the main unit of compilation in Quiklang. |
//! | Module     | Represents a module in Quiklang. A module is a collection of functions, types, and other items that are grouped together. |
//! | ModuleItem | Represents an item within a module. Items can be functions, types, or other entities. |
//! | Visibility | Represents the visibility of a module item. Items can be public, private, or have other levels of visibility. |
//! | Item       | Represents an item within a module. Items can be functions, types, or other entities. |
//! | Function   | Represents a function in Quiklang. Functions are blocks of code that can be called with arguments and return a value. |
//! | Parameter  | Represents a parameter of a function. Parameters have a name and a type. |
//! | Struct     | Represents a struct in Quiklang. Structs are data structures that contain named fields. |
//! | StructField| Represents a field within a struct. Fields have a name and a type. |
//! | Enum       | Represents an enum in Quiklang. Enums are types that can have a fixed set of values. |
//! | EnumVariant| Represents a variant within an enum. Variants have a name and fields. |
//! | EnumField  | Represents the fields of an enum variant. Fields can be tuple or struct. |
//! | TypeAlias  | Represents a type alias in Quiklang. ASTTypeKind aliases are used to alias types. |
//! | Trait      | Represents a trait in Quiklang. Traits are collections of methods that can be implemented by other types. |
//! | TraitBound | Represents a trait bound on a generic type parameter. Trait bounds specify that a generic type parameter must implement a certain trait. |
//! | TraitItem  | Represents an item within a trait. Items can be methods, type aliases, or constants. |
//! | TraitMethod| Represents a method within a trait. Methods are functions that are defined within a trait. |
//! | TraitTypeAlias | Represents a type alias within a trait. ASTTypeKind aliases are used to alias types within a trait. |
//! | TraitConst | Represents a constant within a trait. Constants are defined within a trait. |
//! | Const      | Represents a constant in Quiklang. Constants are immutable values that are defined at compile time. |
//! | Global     | Represents a global variable in Quiklang. Global variables are defined at the module level and can have varying levels of visibility. |

use std::rc::Rc;

use super::{
    expr::{block_expr::BlockExpr, Expr},
    paths::ASTSimplePath,
    types::{ASTGeneric, ASTTypeKind},
};

/// Every binary or library that is compiled with the Quiklang compiler is a `Package`.
/// Packages are the main unit of compilation in Quiklang.
/// A package can contain multiple modules, which are the building blocks of the package.
/// Packages can also reference other packages, locally or from external sources (extern package).
#[derive(Debug, Clone)]
pub struct Package {
    /// The name of the package.
    pub name: String,
    /// The version of the package.
    pub version: String,
    /// The authors of the package.
    pub authors: Vec<String>,
    /// The description of the package.
    pub description: String,
    /// The library module of the package (if any).
    pub library: Option<Module>,
    /// The binary modules of the package (if any).
    pub binaries: Vec<Bin>,
    /// The dependencies of the package.
    pub dependencies: Vec<Package>,
}

/// A bin is a binary module that is compiled as an executable.
#[derive(Debug, Clone)]
pub struct Bin {
    /// The name of the binary module.
    pub name: String,
    /// The module itself.
    pub module: Module,
}

/// A module is a collection of functions, types, and other items that are grouped together.
/// Modules are the building blocks of a package.
#[derive(Debug, Clone)]
pub struct Module {
    /// The name of the module.
    pub name: String,
    /// Items contained in the module.
    pub items: Vec<ModuleItem>,
    /// Nested modules.
    pub submodules: Vec<Module>,
}

/// A module item is a function, type, or other item that is contained within a module.
/// Module items are the individual components of a module.
/// They can be exported or private, depending on their visibility.
#[derive(Debug, Clone)]
pub struct ModuleItem {
    /// The visibility of the module item.
    pub visibility: Visibility,
    /// The item itself.
    pub item: Item,
}

/// The visibility of a module item determines whether it can be accessed from outside the module.
/// Items can be private (only accessible within the module) or public (accessible from other modules).
/// There are different levels of visibility, such as `pub`, `pub(package)`, and `pub(parent)`.
#[derive(Debug, Clone, Copy)]
pub enum Visibility {
    /// Public visibility (`pub`).
    Public,
    /// Public visibility within the package (`pub(package)`).
    Package,
    /// Public visibility to the parent module (`pub(super)`).
    Super,
    /// Private visibility (not specified, default).
    Private,
}

/// An item is a function, type, or other entity that is contained within a module.
#[derive(Debug, Clone)]
pub enum Item {
    /// A function item.
    Function(Function),
    /// A struct item.
    Struct(Struct),
    /// An enum item.
    Enum(Enum),
    /// A type alias item.
    TypeAlias(TypeAlias),
    /// A trait item.
    Trait(Trait),
    /// An Impl item.
    Impl(Impl),
    /// A constant item.
    Const(Const),
    /// A global variable item.
    Global(Global),
    /// A use item.
    Use(Use),
    /// Special Ignore item only for parser.
    Ignore,
}

/// A function is a block of code that can be called with arguments and return a value.
/// Functions can have parameters, a return type, and a body that contains the code to be executed.
/// Functions can also be generic, meaning they can take type parameters.
/// Functions can be defined at the module level or within other functions.
#[derive(Debug, Clone)]
pub struct Function {
    /// The name of the function.
    pub name: String,
    /// The parameters of the function.
    pub parameters: Vec<Parameter>,
    /// The return type of the function.
    pub return_type: ASTTypeKind,
    /// The body of the function.
    pub body: BlockExpr,
}

/// A parameter is a variable that is passed to a function when it is called.
/// Parameters have a name and a type.
/// Parameters can be required or optional, depending on whether they have default values.
/// Parameters can also be generic, meaning they can take type parameters.
#[derive(Debug, Clone)]
pub struct Parameter {
    /// The name of the parameter.
    pub name: String,
    /// The type of the parameter.
    pub ty: ASTTypeKind,
    /// Does the parameter have a default value?
    pub default_value: Option<Expr>,
}

/// A struct is a data structure that contains named fields.
/// If you are reading this, you probably already know what a struct is.
/// Quiklang structs are rust-like.
#[derive(Debug, Clone)]
pub struct Struct {
    /// The name of the struct.
    pub name: String,
    /// The fields of the struct.
    pub fields: Vec<StructField>,
    /// Generics of the struct.
    pub generics: Vec<ASTGeneric>,
}

/// A struct field is a key-type pair that represents a field in a struct.
#[derive(Debug, Clone)]
pub struct StructField {
    /// The name of the field.
    pub name: String,
    /// The type of the field.
    pub ty: ASTTypeKind,
    /// Visibility of the field.
    pub visibility: Visibility,
}

/// An enum is a type that can have a fixed set of values.
/// This is a major selling point of Quiklang, as many VM languages do not have native-level enums
/// Quiklang enums are rust-like.
#[derive(Debug, Clone)]
pub struct Enum {
    /// The name of the enum.
    pub name: String,
    /// The variants of the enum.
    pub variants: Vec<EnumVariant>,
    /// Generics of the enum.
    pub generics: Vec<ASTGeneric>,
}

/// Emum variant, rust-like.
#[derive(Debug, Clone)]
pub struct EnumVariant {
    /// The name of the variant.
    pub name: String,
    /// The fields of the variant.
    pub fields: EnumField,
}

/// Enum field, rust-like.
#[derive(Debug, Clone)]
pub enum EnumField {
    /// No fields.
    None,
    /// Tuple fields.
    Tuple(Vec<ASTTypeKind>),
    /// Struct fields.
    Struct(StructField),
}

/// Types can be aliased using type aliases.
#[derive(Debug, Clone)]
pub struct TypeAlias {
    /// The name of the type alias.
    pub name: String,
    /// The type that the alias refers to.
    pub ty: ASTTypeKind,
    /// Generics of the type alias.
    pub generics: Vec<ASTGeneric>,
}

/// A trait is a collection of methods that can be implemented by other types.
/// Traits are similar to interfaces in other languages.
#[derive(Debug, Clone)]
pub struct Trait {
    /// The name of the trait.
    pub name: &'static str,
    /// The items of the trait.
    pub items: Vec<AssociatedItem>,
    /// Bounds of the trait.
    pub bounds: Vec<TraitBound>,
    /// Generics of the trait.
    pub generics: Vec<ASTGeneric>,
}

/// A trait bound is a constraint on a generic type parameter.
/// Trait bounds specify that a generic type parameter must implement a certain trait.
#[derive(Debug, Clone)]
pub struct TraitBound {
    /// The name of the trait that the generic type parameter must implement.
    pub name: &'static str,
    /// The generics of the trait bound.
    pub generics: Vec<ASTGeneric>,
}

/// Traits can have different types of items
/// Methods, type aliases, and consts.
#[derive(Debug, Clone)]
pub enum AssociatedItem {
    /// A trait method.
    Method(AssociatedMethod),
    /// A type alias.
    TypeAlias(TypeAlias),
    /// A constant.
    Const(Const),
}

/// A trait method is a function that is defined within a trait.
/// Trait methods can be implemented by other types.
#[derive(Debug, Clone)]
pub struct AssociatedMethod {
    /// The name of the method.
    pub name: &'static str,
    /// The parameters of the method.
    pub parameters: Vec<Parameter>,
    /// The return type of the method.
    pub return_type: ASTTypeKind,
    /// Optional body of the method.
    pub body: Option<BlockExpr>,
}

/// A trait type alias is a type alias that is defined within a trait.
/// Trait type aliases can be implemented by other types.
#[derive(Debug, Clone)]
pub struct TraitTypeAlias {
    /// The name of the type alias.
    pub name: &'static str,
    /// The type that the alias refers to.
    pub ty: Option<ASTTypeKind>,
}

/// A trait constant is a constant that is defined within a trait.
/// Trait constants can be implemented by other types.
#[derive(Debug, Clone)]
pub struct TraitConst {
    /// The name of the constant.
    pub name: &'static str,
    /// The type of the constant.
    pub ty: ASTTypeKind,
    /// The value of the constant.
    pub value: Option<Expr>,
}

/// An Impl is a block of code that implements methods or traits for a type.
/// Impls are used to define the behavior of a type.
#[derive(Debug, Clone)]
pub struct Impl {
    /// Self type of the impl.
    pub self_type: ASTTypeKind,
    /// Generics of the impl.
    pub generics: Vec<ASTGeneric>,
    /// Trait implemented by the impl.
    pub trait_impl: Option<Rc<Trait>>,
    /// The items contained in the impl.
    pub items: Vec<AssociatedItem>,
}

/// A const is a constant value that is defined at compile time.
/// Constants are immutable and cannot be changed during runtime.
#[derive(Debug, Clone)]
pub struct Const {
    /// The name of the constant.
    pub name: String,
    /// The Visibility of the constant.
    pub visibility: Visibility,
    /// The type of the constant.
    pub ty: ASTTypeKind,
    /// The value of the constant.
    pub value: Expr,
}

/// A global variable is a variable that is defined at the module level.
/// Global variables have varying levels of visibility.
/// Global variables can be mutable or immutable.
#[derive(Debug, Clone)]
pub struct Global {
    /// The name of the global variable.
    pub name: &'static str,
    /// The visibility of the global variable.
    pub visibility: Visibility,
    /// The type of the global variable.
    pub ty: ASTTypeKind,
    /// Is mutable?
    pub mutable: bool,
    /// The value of the global variable.
    pub value: Expr,
}

/// A use item is used to import items from other modules into the current module.
/// Use items are used to avoid having to fully qualify the names of items.
/// Use items can import individual items, all items from a module, or items with a different name.
/// Use items can also be aliased.
/// Use items are similar to the `use` keyword in Rust.
/// Example: `use std::collections::HashMap as Map;`
#[derive(Debug, Clone)]
pub struct Use {
    /// The path being imported.
    pub path: ASTSimplePath,
    /// The alias of the path (if any).
    pub alias: Option<String>,
    /// Indicate if the path is glob. 'use foo::bar::*'
    pub glob: bool,
}
