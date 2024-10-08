// Symbol (variable) tracker

use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, rc::Rc};

use crate::{backend_vm::qffi::native_fn::NATIVE_FUNCTION_TABLE, frontend::ast::Type};

#[derive(Debug, Clone)]
pub enum SymbolTableType {
    QFFIFunction(isize),
    Function(isize),
    Primitive(isize),
    HeapAllocated(isize, Type), // Register location, type, is_const
}

impl SymbolTableType {
    pub fn safe_unwrap(&self) -> isize {
        match self {
            SymbolTableType::QFFIFunction(i) => *i,
            SymbolTableType::Function(i) => *i,
            SymbolTableType::Primitive(i) => *i,
            SymbolTableType::HeapAllocated(i, _) => *i,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub vars: HashMap<String, SymbolTableType>, // Symbol name, register location.
    pub parent: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut symbol_table = SymbolTable {
            vars: HashMap::new(),
            parent: None,
        };
        for (i, native_fn) in NATIVE_FUNCTION_TABLE.clone().iter().enumerate() {
            let index = SymbolTableType::QFFIFunction(i as isize);
            symbol_table
                .borrow_mut()
                .declare_var(native_fn.name.to_string(), index);
        }
        symbol_table
    }

    pub fn new_with_parent(parent: Rc<RefCell<SymbolTable>>) -> Self {
        SymbolTable {
            vars: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub(super) fn declare_var(&mut self, name: String, reg: SymbolTableType) {
        if let "_" = name.as_str() {
            // _ means intentionally ignored
            return;
        }
        self.vars.insert(name.to_string(), reg);
    }

    pub(super) fn lookup_var(&self, name: &str) -> Option<SymbolTableType> {
        if let Some(reg) = self.vars.get(name) {
            return Some(reg.clone());
        }
        if let Some(ref parent) = self.parent {
            return parent.borrow().lookup_var(name);
        }
        None
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}
