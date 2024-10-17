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
    vars: HashMap<String, (Type, bool)>, // Type, is mutable?
    types: HashMap<String, Type>,
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
        false,
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;
    type_env.declare_var(
        "true".to_string(),
        Type::Bool,
        false,
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;
    type_env.declare_var(
        "false".to_string(),
        Type::Bool,
        false,
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;

    type_env.declare_fn(
        "println",
        Type::Function(vec![(Type::String, false)], Box::new(Type::Null)),
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;

    type_env.declare_fn(
        "time",
        Type::Function(vec![], Box::new(Type::Integer)),
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;

    type_env.declare_fn(
        "panic",
        Type::Function(vec![(Type::Any, false)], Box::new(Type::Null)),
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;

    type_env.declare_fn(
        "forget",
        Type::Function(vec![(Type::Any, false)], Box::new(Type::Null)),
        &Token {
            token: TokenType::EOF,
            line: 0,
            col: 0,
        },
    )?;

    type_env.declare_fn(
        "drop",
        Type::Function(vec![(Type::Any, false)], Box::new(Type::Null)),
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
            types: HashMap::new(),
            parent: None,
        };
        setup_type_env(&mut type_env)?;
        Ok(type_env)
    }

    pub fn new_with_parent(parent: Rc<RefCell<TypeEnvironment>>) -> Self {
        TypeEnvironment {
            vars: HashMap::new(),
            types: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn declare_var(
        &mut self,
        name: String,
        var_type: Type,
        is_mutable: bool,
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
        self.vars.insert(name.to_string(), (var_type, is_mutable));
        Ok(())
    }

    pub fn lookup_var(&self, name: &str) -> Option<Type> {
        if let Some(var_type) = self.vars.get(name) {
            return Some(var_type.0.clone());
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
        self.vars.insert(name.to_string(), (fn_type, false));
        Ok(())
    }

    pub fn lookup_fn(&self, name: &str) -> Option<Type> {
        self.lookup_var(name)
    }

    pub fn declare_struct(
        &mut self,
        name: &str,
        fields: HashMap<String, Type>,
        declaration: &Token,
    ) -> Result<(), ParserError> {
        if self.types.contains_key(name) {
            return Err(ParserError::DeclaredExistingStruct(
                declaration.line,
                declaration.col,
                name.to_string(),
            ));
        }
        self.types
            .insert(name.to_string(), Type::Struct(name.to_string(), fields));
        Ok(())
    }

    pub fn declare_enum(
        &mut self,
        name: &str,
        variants: HashMap<String, Vec<Type>>,
        declaration: &Token,
    ) -> Result<(), ParserError> {
        if self.types.contains_key(name) {
            return Err(ParserError::DeclaredExistingEnum(
                declaration.line,
                declaration.col,
                name.to_string(),
            ));
        }
        self.types
            .insert(name.to_string(), Type::Enum(name.to_string(), variants));
        Ok(())
    }

    pub fn declare_alias(
        &mut self,
        name: &str,
        alias: Box<Type>,
        declaration: &Token,
    ) -> Result<(), ParserError> {
        if self.types.contains_key(name) {
            return Err(ParserError::DeclaredExistingAlias(
                declaration.line,
                declaration.col,
                name.to_string(),
            ));
        }
        self.types
            .insert(name.to_string(), Type::Alias(name.to_string(), alias));
        Ok(())
    }

    pub fn lookup_type(&self, name: &str) -> Option<Type> {
        if let Some(typedef) = self.types.get(name) {
            return Some(typedef.clone());
        }
        if let Some(ref parent) = self.parent {
            return parent.borrow().lookup_type(name);
        }
        None
    }
}
