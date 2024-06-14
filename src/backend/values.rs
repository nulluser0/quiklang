use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::frontend::ast::{Expr, Stmt};

use super::environment::Environment;

#[derive(Debug, PartialEq)]
pub enum ValueType {
    Null,
    String,
    Float,
    Integer,
    Bool,
    Object,
    NativeFunction,
    Function,
    Special, // Stuff like breaks, returns, etc.
}

pub trait RuntimeVal: std::fmt::Debug {
    fn get_type(&self) -> ValueType;
}

#[derive(Debug, PartialEq, Clone)]
pub struct NullVal;

impl RuntimeVal for NullVal {
    fn get_type(&self) -> ValueType {
        ValueType::Null
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringVal {
    pub value: String,
}

impl RuntimeVal for StringVal {
    fn get_type(&self) -> ValueType {
        ValueType::String
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatVal {
    pub value: f64,
}

impl RuntimeVal for FloatVal {
    fn get_type(&self) -> ValueType {
        ValueType::Float
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntegerVal {
    pub value: i64,
}

impl RuntimeVal for IntegerVal {
    fn get_type(&self) -> ValueType {
        ValueType::Integer
    }
}

pub trait ToFloat {
    fn to_float(&self) -> f64;
}

impl ToFloat for IntegerVal {
    fn to_float(&self) -> f64 {
        self.value as f64
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BoolVal {
    pub value: bool,
}

impl RuntimeVal for BoolVal {
    fn get_type(&self) -> ValueType {
        ValueType::Bool
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ObjectVal {
    pub properties: HashMap<String, Option<Val>>,
}

impl RuntimeVal for ObjectVal {
    fn get_type(&self) -> ValueType {
        ValueType::Object
    }
}

type NativeFunctionCallback =
    fn(Vec<Expr>, &Rc<RefCell<Environment>>, &Rc<RefCell<Environment>>) -> Val;

#[derive(Debug, PartialEq, Clone)]
pub struct NativeFunctionVal {
    pub call: NativeFunctionCallback,
}

impl RuntimeVal for NativeFunctionVal {
    fn get_type(&self) -> ValueType {
        ValueType::NativeFunction
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionVal {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Vec<Stmt>,
    pub is_async: bool,
}

impl RuntimeVal for FunctionVal {
    fn get_type(&self) -> ValueType {
        ValueType::Function
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SpecialValKeyword {
    Break,
    Return,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SpecialVal {
    pub keyword: SpecialValKeyword,
    pub return_value: Option<Box<Val>>,
}

impl RuntimeVal for SpecialVal {
    fn get_type(&self) -> ValueType {
        ValueType::Special
    }
}

// This enum encapsulates any RuntimeVal type to handle them generically.
#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    Null(NullVal),
    String(StringVal),
    Float(FloatVal),
    Integer(IntegerVal),
    Bool(BoolVal),
    Object(ObjectVal),
    NativeFunction(NativeFunctionVal),
    Function(Rc<FunctionVal>),
    Special(SpecialVal),
}

impl Val {
    pub fn is_same_type(val1: &Val, val2: &Val) -> Result<(), String> {
        let type1 = val1.get_type();
        let type2 = val2.get_type();

        if type1 != type2 {
            return Err(format!(
                "Type mismatch: {:?} is not the same as {:?}",
                type1, type2
            ));
        }
        Ok(())
    }
}

impl RuntimeVal for Val {
    fn get_type(&self) -> ValueType {
        match self {
            Val::Null(_) => ValueType::Null,
            Val::String(_) => ValueType::String,
            Val::Float(_) => ValueType::Float,
            Val::Integer(_) => ValueType::Integer,
            Val::Bool(_) => ValueType::Bool,
            Val::Object(_) => ValueType::Object,
            Val::NativeFunction(_) => ValueType::NativeFunction,
            Val::Function(_) => ValueType::Function,
            Val::Special(_) => ValueType::Special,
        }
    }
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Null(_) => write!(f, "null"),
            Val::String(StringVal { value }) => write!(f, "{}", value),
            Val::Float(FloatVal { value }) => write!(f, "{}", value),
            Val::Integer(IntegerVal { value }) => write!(f, "{}", value),
            Val::Bool(BoolVal { value }) => write!(f, "{}", value),
            Val::Object(ObjectVal { properties }) => write!(f, "{:?}", properties),
            Val::NativeFunction(NativeFunctionVal { call }) => write!(f, "fn:{:?}", call),
            Val::Function(ref fn_val) => {
                let function_val = fn_val.as_ref();
                write!(f, "fn:{}", function_val.name)
            }
            Val::Special(SpecialVal { keyword, .. }) => write!(f, "{:?}", keyword),
        }
    }
}

// Macros cuz why not
#[macro_export]
macro_rules! mk_null {
    () => {
        Val::Null(NullVal)
    };
}

#[macro_export]
macro_rules! mk_bool {
    ($b:expr) => {
        Val::Bool(BoolVal { value: $b })
    };
    () => {
        mk_bool!(true)
    };
}

#[macro_export]
macro_rules! mk_string {
    ($n:expr) => {
        Val::String(StringVal { value: $n })
    };
}

#[macro_export]
macro_rules! mk_float {
    ($n:expr) => {
        Val::Float(FloatVal { value: $n })
    };
    () => {
        mk_float!(0.0)
    };
}

#[macro_export]
macro_rules! mk_integer {
    ($n:expr) => {
        Val::Integer(IntegerVal { value: $n })
    };
    () => {
        mk_integer!(0)
    };
}

#[macro_export]
macro_rules! mk_native_fn {
    ($n:expr) => {
        Val::NativeFunction(NativeFunctionVal { call: $n })
    };
}
