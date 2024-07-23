// Compiler

use std::collections::HashMap;

use crate::{
    backend_vm::{
        bytecode::{BCIntegrityInfo, BCMetadata, ByteCode},
        instructions::Instruction,
        vm::RegisterVal,
    },
    errors::VMCompileError,
    frontend::ast::Stmt,
};

const fn str_to_byte_array(s: &str) -> [u8; 8] {
    // Convert the string to a byte array with exactly 8 bytes.
    // If the string is shorter, pad with zeros. If it's longer, truncate.
    let mut bytes = [0u8; 8];
    let s_bytes = s.as_bytes();
    let len = if s_bytes.len() > 8 { 8 } else { s_bytes.len() };

    let mut i = 0;
    while i < len {
        bytes[i] = s_bytes[i];
        i += 1;
    }
    bytes
}

pub struct Compiler {
    reg_top: usize,
    constants: Vec<RegisterVal>,
    constant_map: HashMap<RegisterVal, usize>,
    instructions: Vec<Instruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            reg_top: 0,
            constants: Vec::new(),
            constant_map: HashMap::new(),
            instructions: Vec::new(),
        }
    }

    fn reg_top(&self) -> usize {
        self.reg_top
    }

    fn allocate_register(&mut self) -> usize {
        let reg = self.reg_top;
        self.reg_top += 1;
        reg
    }

    fn deallocate_register(&mut self) {
        self.reg_top -= 1;
    }

    fn add_constant(&mut self, constant: RegisterVal) -> usize {
        if let Some(&index) = self.constant_map.get(&constant) {
            return index;
        }
        let index = self.constants.len();
        self.constants.push(constant.clone());
        self.constant_map.insert(constant, index);
        index
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn compile(stmts: Vec<Stmt>) -> Result<ByteCode, VMCompileError> {
        let metadata = BCMetadata {
            ql_version: str_to_byte_array(env!("CARGO_PKG_VERSION")),
            ql_vm_ver: env!("QUIKLANG_VM_VERSION").parse().unwrap(),
            flags: 0,
        };
        let integrity_info = BCIntegrityInfo {
            num_register: 0,
            num_constants: 0,
            num_inst: 0,
            num_string_points: 0,
        };
        let mut bytecode = ByteCode::new(metadata, integrity_info);
        // Generate bytecode from AST
        Self::compile_statements(&mut bytecode, stmts)?;

        Ok(bytecode)
    }

    fn compile_statements(bytecode: &mut ByteCode, stmts: Vec<Stmt>) -> Result<(), VMCompileError> {
        for stmt in stmts {
            Self::compile_statement(bytecode, stmt)?;
        }
        Ok(())
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
