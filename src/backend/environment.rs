// Environment

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::process;
use std::rc::Rc;

use crate::backend::values::{BoolVal, NativeFunctionVal, NullVal, Val};
use crate::{mk_bool, mk_native_fn, mk_null};

use super::native_fn::{native_drop, native_forget, native_println, native_time};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    values: HashMap<String, Val>,
    is_mutable: HashSet<String>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

fn setup_env(env: &mut Environment) {
    // Create Default Global Environment
    env.declare_var("null", mk_null!(), false);
    env.declare_var("true", mk_bool!(true), false);
    env.declare_var("false", mk_bool!(false), false);
    // Define a native built-in method
    env.declare_var("println", mk_native_fn!(native_println), false);
    env.declare_var("time", mk_native_fn!(native_time), false);
    env.declare_var("forget", mk_native_fn!(native_forget), false);
    env.declare_var("drop", mk_native_fn!(native_drop), false);
}

impl Environment {
    pub fn new() -> Self {
        let mut env = Environment {
            values: HashMap::new(),
            is_mutable: HashSet::new(),
            parent: None,
        };
        setup_env(&mut env);
        env
    }

    pub fn new_with_parent(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            is_mutable: HashSet::new(),
            parent: Some(parent),
        }
    }

    pub fn resolve(&self, varname: &str) -> Result<Rc<RefCell<Environment>>, String> {
        if self.values.contains_key(varname) {
            return Ok(Rc::new(RefCell::new(self.clone())));
        }

        if let Some(parent) = &self.parent {
            return parent.borrow().resolve(varname);
        }

        Err(format!(
            "Cannot resolve '{}', as it does not exist.",
            varname
        ))
    }

    pub fn declare_var(&mut self, name: &str, value: Val, is_mutable: bool) -> Val {
        if let std::collections::hash_map::Entry::Vacant(e) = self.values.entry(name.to_string()) {
            e.insert(value.clone());
        } else {
            println!("Cannot declare variable {} as it is already defined.", name);
            process::exit(1);
        }
        if is_mutable {
            self.is_mutable.insert(name.to_owned());
        }
        value
    }

    pub fn assign_var(&mut self, name: &str, value: Val) -> Val {
        let env = self.resolve(name).unwrap_or_else(|_| {
            println!("Cannot resolve {} as it does not exist.", name);
            process::exit(1);
        });

        // immutables (const and let) cannot have its value changed.
        if !env.borrow().is_mutable.contains(name) {
            println!("Cannot resolve {} as it is immutable.", name);
            process::exit(1);
        }

        env.borrow_mut()
            .values
            .insert(name.to_string(), value.clone());
        value
    }

    pub fn drop_var(&mut self, name: &str) -> Val {
        let env = self.resolve(name).unwrap_or_else(|_| {
            println!("Cannot resolve {} as it does not exist.", name);
            process::exit(1);
        });

        let x = env.borrow_mut().values.remove(name).unwrap();
        x
    }

    pub fn lookup_var(&self, name: &str) -> Val {
        let env = self.resolve(name).unwrap_or_else(|_| {
            println!("Cannot resolve {} as it does not exist.", name);
            process::exit(1);
        });
        let x = env.borrow().values.get(name).unwrap().clone();
        x
    }
}
