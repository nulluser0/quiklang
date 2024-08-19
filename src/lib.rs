//! A fast, simple, strongly-typed, and easy-to-use programming language.
//!
//! Quiklang is an universal programming language that is designed to be fast, simple, and easy to use.
//!
//! This file contains the main library module for the Quiklang project.
//!
//! The `lib.rs` file serves as the entry point for the library and defines the various modules
//! that make up the Quiklang project. These modules include:
//!
//! - `backend_interpreter`: Module for the backend interpreter functionality.
//! - `backend_vm`: Module for the backend virtual machine functionality.
//! - `errors`: Module for error handling and error types.
//! - `frontend`: Module for the frontend functionality.
//! - `utils`: Module for utility functions and helper methods.
//!
//! This file acts as a central hub for organizing and accessing the different components of the
//! Quiklang library.
pub mod backend_interpreter;
pub mod backend_vm;
pub mod errors;
pub mod frontend;
pub mod utils;
