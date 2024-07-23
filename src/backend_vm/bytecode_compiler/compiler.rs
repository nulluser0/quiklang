// Compiler

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    backend_vm::{
        bytecode::{BCIntegrityInfo, BCMetadata, ByteCode},
        instructions::Instruction,
        vm::RegisterVal,
    },
    errors::VMCompileError,
    frontend::ast::Stmt,
};

use super::symbol_tracker::SymbolTable;

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
    max_reg: usize,
    reg_top: usize,
    constants: Vec<RegisterVal>,
    constant_map: HashMap<RegisterVal, usize>,
    instructions: Vec<Instruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            max_reg: 0,
            reg_top: 0,
            constants: Vec::new(),
            constant_map: HashMap::new(),
            instructions: Vec::new(),
        }
    }

    pub(super) fn reg_top(&self) -> usize {
        self.reg_top
    }

    pub(super) fn allocate_register(&mut self) -> usize {
        let reg = self.reg_top;
        self.reg_top += 1;
        if self.reg_top > self.max_reg {
            self.max_reg = self.reg_top;
        }
        reg
    }

    pub(super) fn deallocate_register(&mut self) {
        self.reg_top -= 1;
    }

    pub(super) fn add_constant(&mut self, constant: RegisterVal) -> usize {
        if let Some(&index) = self.constant_map.get(&constant) {
            return index;
        }
        let index = self.constants.len();
        self.constants.push(constant.clone());
        self.constant_map.insert(constant, index);
        index
    }

    pub(super) fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn compile(&mut self, stmts: Vec<Stmt>) -> Result<ByteCode, VMCompileError> {
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
        let symbol_table: &Rc<RefCell<SymbolTable>> = &Rc::new(RefCell::new(SymbolTable::new()));
        let mut bytecode = ByteCode::new(metadata, integrity_info);
        // Generate bytecode from AST
        self.compile_statements(stmts, symbol_table)?;

        Ok(bytecode)
    }

    fn compile_statements(
        &mut self,
        stmts: Vec<Stmt>,
        symbol_table: &Rc<RefCell<SymbolTable>>,
    ) -> Result<(), VMCompileError> {
        for stmt in stmts {
            self.compile_statement(stmt, symbol_table)?;
        }
        Ok(())
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        backend_vm::instructions::to_string,
        frontend::ast::{Expr, Literal},
    };

    use super::*;

    #[test]
    fn simple_compile_test() {
        let mut compiler = Compiler::new();
        compiler
            .compile(vec![Stmt::ExprStmt(Expr::Literal(Literal::Integer(
                209309,
            )))])
            .expect("compile fail");

        println!("{:#?}", compiler.constants);

        for inst in compiler.instructions {
            println!("{}", to_string(inst))
        }
    }
}
