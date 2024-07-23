// Symbol (variable) tracker

use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
pub(super) struct SymbolTable {
    vars: HashMap<String, usize>, // Symbol name, register location.
    parent: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    pub(super) fn new() -> Self {
        SymbolTable {
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub(super) fn new_with_parent(parent: Rc<RefCell<SymbolTable>>) -> Self {
        SymbolTable {
            vars: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub(super) fn declare_var(&mut self, name: String, reg: usize) {
        if let "_" = name.as_str() {
            // _ means intentionally ignored
            return;
        }
        self.vars.insert(name.to_string(), reg);
    }

    pub(super) fn lookup_var(&self, name: &str) -> Option<usize> {
        if let Some(reg) = self.vars.get(name) {
            return Some(*reg);
        }
        if let Some(ref parent) = self.parent {
            return parent.borrow().lookup_var(name);
        }
        None
    }
}
