use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::frontend::ast::{FromType, Type};

// Custom types (alias, structs, enums) table
#[derive(Debug, Clone)]
pub struct TypeTable {
    types: HashMap<&'static str, TypeTableEntry>, // Type name
    parent: Option<Rc<RefCell<TypeTable>>>,
}

#[derive(Debug, Clone)]
pub enum TypeTableEntry {
    AliasDefStmt {
        alias: VMCompilerType,
    },
    StructDefStmt {
        key_type_values: HashMap<&'static str, VMCompilerType>,
    },
    EnumDefStmt {
        variants: HashMap<&'static str, Vec<VMCompilerType>>,
    },
}

#[derive(Debug, Clone)]
pub enum VMCompilerType {
    Any,
    String,
    Integer,
    Float,
    Null,
    Bool,
    Function(Vec<VMCompilerType>, Box<VMCompilerType>),
    Array(Box<VMCompilerType>),
    Range(Box<VMCompilerType>),
    Tuple(Vec<VMCompilerType>),
    Custom(String),
}

impl FromType for VMCompilerType {
    type Output = Self;

    fn from_type(type_val: &crate::frontend::ast::Type) -> Option<Self::Output> {
        match type_val {
            Type::Any => Some(Self::Any),
            Type::String => Some(Self::String),
            Type::Integer => Some(Self::Integer),
            Type::Float => Some(Self::Float),
            Type::Object => todo!(),
            Type::Null => Some(Self::Null),
            Type::Bool => Some(Self::Bool),
            Type::Function(params, return_type) => {
                let params: Vec<VMCompilerType> = params
                    .iter()
                    .filter_map(|param| Self::from_type(&param.0))
                    .collect();
                let return_type = Box::new(Self::from_type(return_type)?);
                Some(Self::Function(params, return_type))
            }
            Type::Array(inner_type) => {
                let inner_type = Box::new(Self::from_type(inner_type)?);
                Some(Self::Array(inner_type))
            }
            Type::Range(inner_type) => {
                let inner_type = Box::new(Self::from_type(inner_type)?);
                Some(Self::Range(inner_type))
            }
            Type::Tuple(types) => {
                let types: Vec<VMCompilerType> = types.iter().filter_map(Self::from_type).collect();
                Some(Self::Tuple(types))
            }
            Type::Mismatch => Some(Self::Null),
            Type::Struct(_, _) => todo!(),
            Type::Enum(_, _) => todo!(),
            Type::Alias(_, _) => todo!(),
            Type::Custom(name) => Some(Self::Custom(name.clone())),
        }
    }
}

impl TypeTable {
    pub fn new() -> Self {
        TypeTable {
            types: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Rc<RefCell<TypeTable>>) -> Self {
        TypeTable {
            types: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub(super) fn declare_type(&mut self, name: &'static str, entry: TypeTableEntry) {
        self.types.insert(name, entry);
    }

    pub(super) fn lookup_type(&self, name: &'static str) -> Option<TypeTableEntry> {
        if let Some(entry) = self.types.get(name) {
            return Some(entry.clone());
        }
        if let Some(ref parent) = self.parent {
            return parent.borrow().lookup_type(name);
        }
        None
    }
}

impl Default for TypeTable {
    fn default() -> Self {
        Self::new()
    }
}
