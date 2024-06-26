// Type environment
// For type checking

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::errors::ParserError;

use super::{ast::Type, lexer::Token};

#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    types: HashMap<String, Type>,
    parent: Option<Rc<RefCell<TypeEnvironment>>>,
}

impl Default for TypeEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeEnvironment {
    pub fn new() -> Self {
        TypeEnvironment {
            types: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Rc<RefCell<TypeEnvironment>>) -> Self {
        TypeEnvironment {
            types: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn declare_var(
        &mut self,
        name: String,
        var_type: Type,
        token: &Token,
    ) -> Result<(), ParserError> {
        if self.types.contains_key(name.as_str()) {
            return Err(ParserError::DeclaredExistingVariable(
                token.line,
                token.col,
                name.to_string(),
            ));
        }
        self.types.insert(name.to_string(), var_type);
        Ok(())
    }

    pub fn lookup_var(&self, name: &str) -> Option<Type> {
        if let Some(var_type) = self.types.get(name) {
            return Some(var_type.clone());
        }
        if let Some(ref parent) = self.parent {
            return parent.borrow().lookup_var(name);
        }
        None
    }

    pub fn declare_fn(
        &mut self,
        name: &str,
        fn_type: Type,
        declaration: &Token,
    ) -> Result<(), ParserError> {
        if self.types.contains_key(name) {
            return Err(ParserError::DeclaredExistingFunction(
                declaration.line,
                declaration.col,
                name.to_string(),
            ));
        }
        self.types.insert(name.to_string(), fn_type);
        Ok(())
    }

    pub fn lookup_fn(&self, name: &str) -> Option<Type> {
        self.lookup_var(name)
    }
}
