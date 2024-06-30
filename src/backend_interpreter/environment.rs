// Environment

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::process;
use std::rc::Rc;

use crate::backend_interpreter::values::{BoolVal, NativeFunctionVal, NullVal, Val};
use crate::errors::RuntimeError;
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
        Self::new().expect("Unable to create a new root Environment. This should not happen. If this still occur, please report this issue!")
    }
}

fn setup_env(env: &mut Environment) -> Result<(), RuntimeError> {
    // Create Default Global Environment
    env.declare_var("null", mk_null!(), false)?;
    env.declare_var("true", mk_bool!(true), false)?;
    env.declare_var("false", mk_bool!(false), false)?;
    // Define a native built-in method
    env.declare_var("println", mk_native_fn!(native_println), false)?;
    env.declare_var("time", mk_native_fn!(native_time), false)?;
    env.declare_var("forget", mk_native_fn!(native_forget), false)?;
    env.declare_var("drop", mk_native_fn!(native_drop), false)?;
    Ok(())
}

impl Environment {
    pub fn new() -> Result<Self, RuntimeError> {
        let mut env = Environment {
            values: HashMap::new(),
            is_mutable: HashSet::new(),
            parent: None,
        };
        setup_env(&mut env)?;
        Ok(env)
    }

    pub fn get_parent(&self) -> Result<Rc<RefCell<Environment>>, String> {
        match &self.parent {
            Some(result) => Ok(result.clone()),
            None => Err("No parent found!".to_string()),
        }
    }

    pub fn new_with_parent(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            is_mutable: HashSet::new(),
            parent: Some(parent),
        }
    }

    pub fn resolve(
        env: &Rc<RefCell<Environment>>,
        varname: &str,
    ) -> Result<Rc<RefCell<Environment>>, RuntimeError> {
        if env.borrow().values.contains_key(varname) {
            return Ok(env.clone());
        }

        if let Some(parent) = &env.borrow().parent {
            return Self::resolve(parent, varname);
        }

        Err(RuntimeError::UndefinedVariable(varname.to_string()))
    }

    pub fn declare_var(
        &mut self,
        name: &str,
        value: Val,
        is_mutable: bool,
    ) -> Result<Val, RuntimeError> {
        if let "_" = name {
            // _ means intentionally ignored
            return Ok(value);
        }
        if let std::collections::hash_map::Entry::Vacant(e) = self.values.entry(name.to_string()) {
            e.insert(value.clone());
        } else {
            return Err(RuntimeError::DeclaredExistingVariable(name.to_string()));
        }
        if is_mutable {
            self.is_mutable.insert(name.to_owned());
        }
        Ok(value)
    }

    pub fn assign_var(
        env: &Rc<RefCell<Environment>>,
        name: &str,
        value: Val,
    ) -> Result<Val, RuntimeError> {
        let containing_env = Self::resolve(env, name)?;

        // immutables (const and let) cannot have its value changed.
        if !containing_env.borrow().is_mutable.contains(name) {
            return Err(RuntimeError::ImmutableVariableEdit(name.to_string()));
        }

        match Val::is_same_type(containing_env.borrow().values.get(name).unwrap(), &value) {
            Ok(()) => {}
            Err(e) => {
                println!("{}", e);
                process::exit(1);
            }
        }

        containing_env
            .borrow_mut()
            .values
            .insert(name.to_string(), value.clone());
        Ok(value)
    }

    pub fn drop_var(env: &Rc<RefCell<Environment>>, name: &str) {
        let containing_env = Self::resolve(env, name).unwrap_or_else(|_| {
            println!("Cannot resolve {} as it does not exist.", name);
            process::exit(1);
        });

        containing_env.borrow_mut().values.remove(name).unwrap();
    }

    pub fn lookup_var(env: &Rc<RefCell<Environment>>, name: &str) -> Result<Val, RuntimeError> {
        let env = Self::resolve(env, name)?;
        let x = env.borrow().values.get(name).unwrap().clone();
        Ok(x)
    }
}
