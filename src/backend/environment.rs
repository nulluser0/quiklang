// Environment

use std::collections::{HashMap, HashSet};
use std::process;

use crate::backend::values::Val;

#[derive(Debug, Clone)]
pub struct Environment<'a> {
    values: HashMap<String, Val>,
    is_mutable: HashSet<String>,
    parent: Option<&'a Environment<'a>>,
}

impl<'a> Default for Environment<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            is_mutable: HashSet::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: &'a Environment<'a>) -> Self {
        Environment {
            values: HashMap::new(),
            is_mutable: HashSet::new(),
            parent: Some(parent),
        }
    }

    pub fn resolve(&'a self, varname: &str) -> Result<&'a Environment<'a>, String> {
        if self.values.contains_key(varname) {
            return Ok(self);
        }

        if let Some(parent) = self.parent {
            return parent.resolve(varname);
        }

        Err(format!("Cannot resolve '{}', as it does not exist.", varname))
    }

    pub fn declare_var(&mut self, name: &str, value: Val, is_mutable: bool) -> Val {
        if self.values.contains_key(name) {
            println!("Cannot declare variable {} as it is already defined.", name);
            process::exit(1);
        }
        self.values.insert(name.to_string(), value.clone());
        if is_mutable {
            self.is_mutable.insert(name.to_owned());
        }
        value
    }

    pub fn assign_var(&mut self, name: &str, value: Val) -> Val {
        match self.resolve(name) {
            Ok(result) => result,
            Err(_) => {
                println!("Cannot resolve {} as it does not exist.", name);
                process::exit(1);
            }
        };

        // immutables (const and let) cannot have its value changed.
        if !self.is_mutable.contains(name) {
            println!("Cannot resolve {} as it is immutable.", name);
            process::exit(1);
        }

        self.values.insert(name.to_string(), value.clone());
        value
    }

    pub fn lookup_var(&mut self, name: &str) -> Val {
        let env = match self.resolve(name) {
            Ok(result) => result,
            Err(_) => {
                println!("Cannot resolve {} as it does not exist.", name);
                process::exit(1);
            }
        };
        env.values.get(name).unwrap().clone()
    }
}
