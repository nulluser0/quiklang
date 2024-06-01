use std::collections::HashMap;

use super::environment::Environment;

#[derive(Debug, PartialEq)]
pub enum ValueType {
    Null,
    Float,
    Integer,
    Bool,
    Object,
    NativeFunction,
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

type NativeFunctionCallback = fn(Vec<Val>, &mut Environment) -> Val;

#[derive(Debug, PartialEq, Clone)]
pub struct NativeFunctionVal {
    pub call: NativeFunctionCallback,
}

impl RuntimeVal for NativeFunctionVal {
    fn get_type(&self) -> ValueType {
        ValueType::NativeFunction
    }
}

// This enum encapsulates any RuntimeVal type to handle them generically.
#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    Null(NullVal),
    Float(FloatVal),
    Integer(IntegerVal),
    Bool(BoolVal),
    Object(ObjectVal),
    NativeFunction(NativeFunctionVal),
}

impl RuntimeVal for Val {
    fn get_type(&self) -> ValueType {
        match self {
            Val::Null(_) => ValueType::Null,
            Val::Float(_) => ValueType::Float,
            Val::Integer(_) => ValueType::Integer,
            Val::Bool(_) => ValueType::Bool,
            Val::Object(_) => ValueType::Object,
            Val::NativeFunction(_) => ValueType::NativeFunction,
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
