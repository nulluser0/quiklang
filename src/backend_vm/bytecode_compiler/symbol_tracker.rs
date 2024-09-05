// Symbol (variable) tracker

use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, rc::Rc};

use crate::backend_vm::qffi::native_fn::NATIVE_FUNCTION_TABLE;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    vars: HashMap<String, isize>, // Symbol name, register location. -ve values indicate QFFI function.
    parent: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    pub(super) fn new() -> Self {
        let mut symbol_table = SymbolTable {
            vars: HashMap::new(),
            parent: None,
        };
        for (i, native_fn) in NATIVE_FUNCTION_TABLE.clone().iter().enumerate() {
            let index = -1 - (i as isize);
            symbol_table
                .borrow_mut()
                .declare_var(native_fn.name.to_string(), index);
        }
        symbol_table
    }

    pub(super) fn new_with_parent(parent: Rc<RefCell<SymbolTable>>) -> Self {
        SymbolTable {
            vars: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub(super) fn declare_var(&mut self, name: String, reg: isize) {
        if let "_" = name.as_str() {
            // _ means intentionally ignored
            return;
        }
        self.vars.insert(name.to_string(), reg);
    }

    pub(super) fn lookup_var(&self, name: &str) -> Option<isize> {
        if let Some(reg) = self.vars.get(name) {
            return Some(*reg);
        }
        if let Some(ref parent) = self.parent {
            return parent.borrow().lookup_var(name);
        }
        None
    }
}
