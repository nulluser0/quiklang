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
//! | TypeAlias  | Represents a type alias in Quiklang. Type aliases are used to alias types. |
//! | Trait      | Represents a trait in Quiklang. Traits are collections of methods that can be implemented by other types. |
//! | TraitBound | Represents a trait bound on a generic type parameter. Trait bounds specify that a generic type parameter must implement a certain trait. |
//! | TraitItem  | Represents an item within a trait. Items can be methods, type aliases, or constants. |
//! | TraitMethod| Represents a method within a trait. Methods are functions that are defined within a trait. |
//! | TraitTypeAlias | Represents a type alias within a trait. Type aliases are used to alias types within a trait. |
//! | TraitConst | Represents a constant within a trait. Constants are defined within a trait. |
//! | Const      | Represents a constant in Quiklang. Constants are immutable values that are defined at compile time. |
//! | Global     | Represents a global variable in Quiklang. Global variables are defined at the module level and can have varying levels of visibility. |

/// Every binary or library that is compiled with the Quiklang compiler is a `Package`.
/// Packages are the main unit of compilation in Quiklang.
/// A package can contain multiple modules, which are the building blocks of the package.
/// Packages can also reference other packages, locally or from external sources (extern package).
pub struct Package {
    /// The name of the package.
    pub name: &'static str,
    /// The version of the package.
    pub version: &'static str,
    /// The authors of the package.
    pub authors: Vec<&'static str>,
    /// The description of the package.
    pub description: &'static str,
    /// The modules contained in the package.
    pub modules: Vec<Module>,
    /// The dependencies of the package.
    pub dependencies: Vec<Package>,
}

/// A module is a collection of functions, types, and other items that are grouped together.
/// Modules are the building blocks of a package.
pub struct Module {
    /// The name of the module.
    pub name: &'static str,
    /// Items contained in the module.
    pub items: Vec<ModuleItem>,
}

/// A module item is a function, type, or other item that is contained within a module.
/// Module items are the individual components of a module.
/// They can be exported or private, depending on their visibility.
pub struct ModuleItem {
    /// The visibility of the module item.
    pub visibility: Visibility,
    /// The item itself.
    pub item: Item,
}

/// The visibility of a module item determines whether it can be accessed from outside the module.
/// Items can be private (only accessible within the module) or public (accessible from other modules).
/// There are different levels of visibility, such as `pub`, `pub(package)`, and `pub(parent)`.
pub enum Visibility {
    /// Public visibility (`pub`).
    Public,
    /// Public visibility within the package (`pub(package)`).
    Package,
    /// Public visibility to the parent module (`pub(parent)`).
    Super,
    /// Private visibility (not specified, default).
    Private,
}

/// An item is a function, type, or other entity that is contained within a module.
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
    /// A constant item.
    Const(Const),
    /// A global variable item.
    Global(Global),
}

/// A function is a block of code that can be called with arguments and return a value.
/// Functions can have parameters, a return type, and a body that contains the code to be executed.
/// Functions can also be generic, meaning they can take type parameters.
/// Functions can be defined at the module level or within other functions.
pub struct Function {
    /// The name of the function.
    pub name: &'static str,
    /// The parameters of the function.
    pub parameters: Vec<Parameter>,
    /// The return type of the function.
    pub return_type: Type,
    /// The body of the function.
    pub body: BlockStmt,
}

/// A parameter is a variable that is passed to a function when it is called.
/// Parameters have a name and a type.
/// Parameters can be required or optional, depending on whether they have default values.
/// Parameters can also be generic, meaning they can take type parameters.
pub struct Parameter {
    /// The name of the parameter.
    pub name: &'static str,
    /// The type of the parameter.
    pub ty: Type,
    /// Whether the parameter is optional.
    pub optional: bool,
}

/// A struct is a data structure that contains named fields.
/// If you are reading this, you probably already know what a struct is.
/// Quiklang structs are rust-like.
pub struct Struct {
    /// The name of the struct.
    pub name: &'static str,
    /// The fields of the struct.
    pub fields: Vec<StructField>,
    /// Generics of the struct.
    pub generics: Vec<Generic>,
}

/// A struct field is a key-type pair that represents a field in a struct.
pub struct StructField {
    /// The name of the field.
    pub name: &'static str,
    /// The type of the field.
    pub ty: Type,
    /// Visibility of the field.
    pub visibility: Visibility,
}

/// An enum is a type that can have a fixed set of values.
/// This is a major selling point of Quiklang, as many VM languages do not have native-level enums
/// Quiklang enums are rust-like.
pub struct Enum {
    /// The name of the enum.
    pub name: &'static str,
    /// The variants of the enum.
    pub variants: Vec<EnumVariant>,
    /// Generics of the enum.
    pub generics: Vec<Generic>,
}

/// Emum variant, rust-like.
pub struct EnumVariant {
    /// The name of the variant.
    pub name: &'static str,
    /// The fields of the variant.
    pub fields: EnumField,
}

/// Enum field, rust-like.
pub enum EnumField {
    /// No fields.
    None,
    /// Tuple fields.
    Tuple(Vec<Type>),
    /// Struct fields.
    Struct(StructField),
}

/// Types can be aliased using type aliases.
pub struct TypeAlias {
    /// The name of the type alias.
    pub name: &'static str,
    /// The type that the alias refers to.
    pub ty: Type,
    /// Generics of the type alias.
    pub generics: Vec<Generic>,
}

/// A trait is a collection of methods that can be implemented by other types.
/// Traits are similar to interfaces in other languages.
pub struct Trait {
    /// The name of the trait.
    pub name: &'static str,
    /// The items of the trait.
    pub items: Vec<TraitItem>,
    /// Bounds of the trait.
    pub bounds: Vec<TraitBound>,
    /// Generics of the trait.
    pub generics: Vec<Generic>,
}

/// A trait bound is a constraint on a generic type parameter.
/// Trait bounds specify that a generic type parameter must implement a certain trait.
pub struct TraitBound {
    /// The name of the trait that the generic type parameter must implement.
    pub name: &'static str,
    /// The generics of the trait bound.
    pub generics: Vec<Generic>,
}

/// Traits can have different types of items
/// Methods, type aliases, and consts.
pub enum TraitItem {
    /// A trait method.
    Method(TraitMethod),
    /// A type alias.
    TypeAlias(TypeAlias),
    /// A constant.
    Const(Const),
}

/// A trait method is a function that is defined within a trait.
/// Trait methods can be implemented by other types.
pub struct TraitMethod {
    /// The name of the method.
    pub name: &'static str,
    /// The parameters of the method.
    pub parameters: Vec<Parameter>,
    /// The return type of the method.
    pub return_type: Type,
    /// Optional body of the method.
    pub body: Option<BlockStmt>,
}

/// A trait type alias is a type alias that is defined within a trait.
/// Trait type aliases can be implemented by other types.
pub struct TraitTypeAlias {
    /// The name of the type alias.
    pub name: &'static str,
    /// The type that the alias refers to.
    pub ty: Option<Type>,
}

/// A trait constant is a constant that is defined within a trait.
/// Trait constants can be implemented by other types.
pub struct TraitConst {
    /// The name of the constant.
    pub name: &'static str,
    /// The type of the constant.
    pub ty: Type,
    /// The value of the constant.
    pub value: Option<Expr>,
}

/// A const is a constant value that is defined at compile time.
/// Constants are immutable and cannot be changed during runtime.
pub struct Const {
    /// The name of the constant.
    pub name: &'static str,
    /// The type of the constant.
    pub ty: Type,
    /// The value of the constant.
    pub value: Expr,
}

/// A global variable is a variable that is defined at the module level.
/// Global variables have varying levels of visibility.
/// Global variables can be mutable or immutable.
pub struct Global {
    /// The name of the global variable.
    pub name: &'static str,
    /// The visibility of the global variable.
    pub visibility: Visibility,
    /// The type of the global variable.
    pub ty: Type,
    /// Is mutable?
    pub mutable: bool,
    /// The value of the global variable.
    pub value: Expr,
}