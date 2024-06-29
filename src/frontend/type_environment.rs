// Type environment
// For type checking

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::errors::ParserError;

use super::{
    ast::Type,
    lexer::{Token, TokenType},
};

#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    vars: HashMap<String, Type>,
    parent: Option<Rc<RefCell<TypeEnvironment>>>,
}

impl Default for TypeEnvironment {
    fn default() -> Self {
        Self::new().expect("Unable to create a new root Type Environment. This should not happen. If this still occur, please report this issue!")
    }
}

fn setup_type_env(type_env: &mut TypeEnvironment) -> Result<(), ParserError> {
    // Variables
    type_env.declare_var(
        "null".to_string(),
        Type::Null,
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;
    type_env.declare_var(
        "true".to_string(),
        Type::Bool,
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;
    type_env.declare_var(
        "false".to_string(),
        Type::Bool,
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;

    type_env.declare_fn(
        "println",
        Type::Null,
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;

    type_env.declare_fn(
        "time",
        Type::Integer,
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;

    type_env.declare_fn(
        "forget",
        Type::Null,
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;

    type_env.declare_fn(
        "drop",
        Type::Null,
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;

    Ok(())
}

impl TypeEnvironment {
    pub fn new() -> Result<Self, ParserError> {
        let mut type_env = TypeEnvironment {
            vars: HashMap::new(),
            parent: None,
        };
        setup_type_env(&mut type_env)?;
        Ok(type_env)
    }

    pub fn new_with_parent(parent: Rc<RefCell<TypeEnvironment>>) -> Self {
        TypeEnvironment {
            vars: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn declare_var(
        &mut self,
        name: String,
        var_type: Type,
        token: &Token,
    ) -> Result<(), ParserError> {
        if let "_" = name.as_str() {
            // _ means intentionally ignored
            return Ok(());
        }
        if self.vars.contains_key(name.as_str()) {
            return Err(ParserError::DeclaredExistingVariable(
                token.line,
                token.col,
                name.to_string(),
            ));
        }
        self.vars.insert(name.to_string(), var_type);
        Ok(())
    }

    pub fn lookup_var(&self, name: &str) -> Option<Type> {
        if let Some(var_type) = self.vars.get(name) {
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
        if self.vars.contains_key(name) {
            return Err(ParserError::DeclaredExistingFunction(
                declaration.line,
                declaration.col,
                name.to_string(),
            ));
        }
        self.vars.insert(name.to_string(), fn_type);
        Ok(())
    }

    pub fn lookup_fn(&self, name: &str) -> Option<Type> {
        self.lookup_var(name)
    }
}
