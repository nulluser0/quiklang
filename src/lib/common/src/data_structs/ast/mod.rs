//! # AST
//!
//! The AST module contains the Abstract Syntax Tree data structures for the Quiklang project.
//!
//! ## Modules
//!
//! | Module                                 | Description                                 |
//! |----------------------------------------|---------------------------------------------|
//! | [`expr`](./expr/index.html)            | Module for expression AST structures.       |
//! | [`package_module`](./package_module/index.html) | Module for packages and modules AST structures.           |
//! | [`stmt`](./stmt/index.html)            | Module for statement AST structures.         |
//! | [`types`](./types/index.html)          | Module for type AST structures.             |

pub mod expr;
pub mod package_metadata;
pub mod package_module;
pub mod paths;
pub mod stmt;
pub mod types;
