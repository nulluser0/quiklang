// Environment

use std::collections::HashMap;
use std::process;
use std::rc::Rc;
use std::cell::RefCell;

use crate::backend::values::Val;

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, Val>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn resolve(&self, name: &str) -> Result<Environment, ()> {
        match self.values.get(name) {
            Some(_) => Ok(self.clone()),
            None => {
                if let Some(ref parent) = self.parent {
                    parent.borrow().resolve(name)
                } else {
                    return Err(())
                }
            }
        }
    }

    pub fn declare_var(&mut self, name: &str, value: Val) -> Val {
        if self.values.contains_key(name) {
            println!("Cannot declare variable {} as it is already defined.", name);
            process::exit(1);
        }
        self.values.insert(name.to_string(), value.clone());
        value
    }

    pub fn assign_var(&mut self, name: &str, value: Val) -> Val {
        let mut env = match self.resolve(&name) {
            Ok(result) => result,
            Err(_) => {
                println!("Cannot resolve {} as it does not exist.", name);
                process::exit(1);
            }
        };
        env.values.insert(name.to_string(), value.clone());
        value
    }

    pub fn lookup_var(&mut self, name: &str) -> Val {
        let env = match self.resolve(&name) {
            Ok(result) => result,
            Err(_) => {
                println!("Cannot resolve {} as it does not exist.", name);
                process::exit(1);
            }
        };
        env.values.get(name).unwrap().clone()
    }
}