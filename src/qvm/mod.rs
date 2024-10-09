//! # QVM (Quiklang Virtual Machine)
//!
//! The QVM module is responsible for executing the bytecode generated by the compiler.
//!
//! [`Return to Quiklang Crate`](../index.html)
//!
//! ## Modules
//!
//! | Module                                 | Description                                 |
//! |----------------------------------------|---------------------------------------------|
//! | [`vm`](./vm/index.html)                | Module for the virtual machine. This module contains the virtual machine and its components. |
//! | [`bytecode`](./bytecode/index.html)    | Module for the bytecode format. This module contains the bytecode format and functions for encoding and decoding bytecode. |
//! | [`instructions`](./instructions/index.html) | Module for the instructions. This module contains the instruction set for the Quiklang Virtual Machine. |
//! | [`qffi`](./qffi/index.html)            | Module for the Quiklang Foreign Function Interface (QFFI). This module contains the functions for interfacing with the host environment. |
//! | [`register_val`](./register_val/index.html) | Module for the register values. This module contains the register values used by the virtual machine. |
//!
//! ---
//!
//! **List of modules in alphabetical order:**
//!
//! - [`bytecode`](./bytecode/index.html): Module for the bytecode format.
//! - [`instructions`](./instructions/index.html): Module for the instructions.
//! - [`qffi`](./qffi/index.html): Module for the Quiklang Foreign Function Interface (QFFI).
//! - [`register_val`](./register_val/index.html): Module for the register values.
//! - [`vm`](./vm/index.html): Module for the virtual machine.

pub mod bytecode;
pub mod bytecode_compiler;
pub mod instructions;
pub mod qffi;
pub mod register_val;
pub mod vm;
