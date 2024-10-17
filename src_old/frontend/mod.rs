//! # Frontend
//!
//! The frontend module contains the lexer, parser, AST and type environment for the Quiklang project.
//!
//! [`Return to Quiklang Crate`](../index.html)
//!
//! ## Modules
//!
//! | Module                                 | Description                                 |
//! |----------------------------------------|---------------------------------------------|
//! | [`lexer`](./lexer/index.html)          | Module for the lexer.                       |
//! | [`parser`](./parser/index.html)        | Module for the parser.                      |
//! | [`semantic_analysis`](./semantic_analysis/index.html) | Module for semantic analysis, including type checking. |
//! | [`ast`](./ast/index.html)              | Module for the Abstract Syntax Tree (AST). |
//! | [`type_environment`](./type_environment/index.html) | Module for the type environment. |
//!
//! ---
//!
//! **List of modules in alphabetical order:**
//!
//! - [`ast`](./ast/index.html): Module for the Abstract Syntax Tree (AST).
//! - [`lexer`](./lexer/index.html): Module for the lexer.
//! - [`parser`](./parser/index.html): Module for the parser.
//! - [`semantic_analysis`](./semantic_analysis/index.html): Module for semantic analysis, including type checking.
//! - [`type_environment`](./type_environment/index.html): Module for the type environment.

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod type_environment;
