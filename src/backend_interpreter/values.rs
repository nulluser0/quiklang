use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    errors::InterpreterError,
    frontend::ast::{Expr, FromType, Stmt, Type},
};

use super::environment::Environment;

#[derive(Debug, PartialEq, Clone)]
pub enum ValueType {
    Null,
    String,
    Float,
    Integer,
    Bool,
    Object,
    NativeFunction,
    Function,
    Array(Box<ValueType>),
    Range(Box<ValueType>),
    Tuple(Vec<ValueType>),
    Iterator(Box<ValueType>),
    Special, // Stuff like breaks, returns, etc.

    // SHOULD NOT BE USED!
    Any,
}

impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Null => write!(f, "Null"),
            ValueType::String => write!(f, "String"),
            ValueType::Float => write!(f, "Float"),
            ValueType::Integer => write!(f, "Integer"),
            ValueType::Bool => write!(f, "Bool"),
            ValueType::Object => write!(f, "Object"),
            ValueType::NativeFunction => write!(f, "NativeFunction"),
            ValueType::Function => write!(f, "Function"),
            ValueType::Array(inner) => write!(f, "Array<{}>", inner),
            ValueType::Range(inner) => write!(f, "Range<{}>", inner),
            ValueType::Tuple(inner) => {
                let elements: Vec<String> = inner.iter().map(|val| format!("{}", val)).collect();
                write!(f, "({})", elements.join(", "))
            }
            ValueType::Iterator(inner) => write!(f, "Iterator<{}>", inner),
            ValueType::Special => write!(f, "Special"),
            ValueType::Any => write!(f, "_"),
        }
    }
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

type NativeFunctionCallback = fn(
    Vec<(Expr, bool)>,
    &Rc<RefCell<Environment>>,
    &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError>;

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
    pub parameters: Vec<(String, ValueType, bool)>,
    pub body: Vec<Stmt>,
    pub is_async: bool,
}

impl RuntimeVal for FunctionVal {
    fn get_type(&self) -> ValueType {
        ValueType::Function
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayVal {
    pub values: Vec<Val>,
    pub inner_type: ValueType,
}

impl RuntimeVal for ArrayVal {
    fn get_type(&self) -> ValueType {
        ValueType::Array(Box::new(self.inner_type.clone()))
    }
}

impl std::fmt::Display for ArrayVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elements: Vec<String> = self.values.iter().map(|val| format!("{}", val)).collect();
        write!(f, "[{}]", elements.join(", "))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct RangeVal {
    pub start: Box<Val>,
    pub end: Box<Val>,
    pub inclusive: bool,
    pub inner_type: ValueType,
}

impl RuntimeVal for RangeVal {
    fn get_type(&self) -> ValueType {
        ValueType::Range(Box::new(self.inner_type.clone()))
    }
}

impl std::fmt::Display for RangeVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.start,
            if self.inclusive { "..=" } else { ".." },
            self.end
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleVal {
    pub values: Vec<Val>,
}

impl RuntimeVal for TupleVal {
    fn get_type(&self) -> ValueType {
        ValueType::Tuple(self.values.iter().map(|val| val.get_type()).collect())
    }
}

impl std::fmt::Display for TupleVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elements: Vec<String> = self.values.iter().map(|val| format!("{}", val)).collect();
        write!(f, "({})", elements.join(", "))
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

pub trait Iterator: std::fmt::Debug {
    fn next(&mut self) -> Option<Val>;
}

#[derive(Debug, Clone)]
pub struct IteratorVal {
    pub iterator: Rc<RefCell<dyn Iterator>>,
    pub return_type: ValueType,
}

impl PartialEq for IteratorVal {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.iterator, &other.iterator)
    }
}

impl RuntimeVal for IteratorVal {
    fn get_type(&self) -> ValueType {
        ValueType::Iterator(Box::new(self.return_type.clone()))
    }
}

#[derive(Debug)]
pub struct RangeIterator {
    start: i64,
    end: i64,
    inclusive: bool,
}

impl RangeIterator {
    pub fn new(start: i64, end: i64, inclusive: bool) -> Self {
        Self {
            start,
            end,
            inclusive,
        }
    }
}

impl Iterator for RangeIterator {
    fn next(&mut self) -> Option<Val> {
        if self.inclusive {
            if self.start <= self.end {
                let value = self.start;
                self.start += 1;
                Some(Val::Integer(IntegerVal { value }))
            } else {
                None
            }
        } else if self.start < self.end {
            let value = self.start;
            self.start += 1;
            Some(Val::Integer(IntegerVal { value }))
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct ArrayIterator {
    array: Vec<Val>,
    index: usize,
}

impl ArrayIterator {
    pub fn new(array: Vec<Val>) -> Self {
        Self { array, index: 0 }
    }
}

impl Iterator for ArrayIterator {
    fn next(&mut self) -> Option<Val> {
        if self.index < self.array.len() {
            let value = self.array[self.index].clone();
            self.index += 1;
            Some(value)
        } else {
            None
        }
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
    Array(ArrayVal),
    Range(RangeVal),
    Tuple(TupleVal),
    Special(SpecialVal),
    Iterator(IteratorVal),
}

impl FromType for Val {
    type Output = ValueType;
    fn from_type(type_val: &crate::frontend::ast::Type) -> Option<Self::Output> {
        match type_val {
            Type::Any => Some(ValueType::Any),
            Type::String => Some(ValueType::String),
            Type::Integer => Some(ValueType::Integer),
            Type::Float => Some(ValueType::Float),
            Type::Object => Some(ValueType::Object),
            Type::Null => Some(ValueType::Null),
            Type::Bool => Some(ValueType::Bool),
            Type::Array(inner) => Some(ValueType::Array(Box::new(Val::from_type(inner)?))),
            Type::Range(inner) => Some(ValueType::Array(Box::new(Val::from_type(inner)?))),
            Type::Tuple(inner_types) => {
                let values: Vec<ValueType> = inner_types
                    .iter()
                    .map(|inner_type| Val::from_type(inner_type).unwrap())
                    .collect();
                Some(ValueType::Tuple(values))
            }
            Type::Mismatch => None,
            Type::Function(_, _) => Some(ValueType::Function),
            Type::Struct(_, _) => todo!(),
            Type::Enum(_, _) => todo!(),
            Type::Alias(_, _) => todo!(),
            Type::Custom(_) => todo!(),
        }
    }
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

    pub fn to_i64(&self) -> Result<i64, InterpreterError> {
        match self {
            Val::Integer(IntegerVal { value }) => Ok(*value),
            _ => Err(InterpreterError::TypeError {
                message: "Value is not an integer.".to_string(),
                expected: ValueType::Integer,
                found: self.get_type(),
            }),
        }
    }
}

impl Val {
    pub fn to_iterator(self) -> Result<Rc<RefCell<dyn Iterator>>, InterpreterError> {
        match self {
            Val::Array(array_val) => {
                Ok(Rc::new(RefCell::new(ArrayIterator::new(array_val.values))))
            }
            Val::Range(range_val) => Ok(Rc::new(RefCell::new(RangeIterator::new(
                range_val.start.to_i64()?,
                range_val.end.to_i64()?,
                range_val.inclusive,
            )))),
            _ => Err(InterpreterError::TypeError {
                message: "Value is not iterable.".to_string(),
                expected: ValueType::Iterator(Box::new(ValueType::Any)),
                found: self.get_type(),
            }),
        }
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
            Val::Array(ArrayVal {
                values: _,
                inner_type,
            }) => ValueType::Array(Box::new(inner_type.clone())),
            Val::Range(RangeVal { inner_type, .. }) => {
                ValueType::Range(Box::new(inner_type.clone()))
            }
            Val::Tuple(inner) => inner.get_type(),
            Val::Iterator(IteratorVal {
                iterator: _,
                return_type,
            }) => ValueType::Iterator(Box::new(return_type.clone())),
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
            Val::Array(values) => write!(f, "{}", values),
            Val::Range(values) => write!(f, "{}", values),
            Val::Tuple(values) => write!(f, "{}", values),
            Val::Iterator(IteratorVal {
                iterator,
                return_type: _,
            }) => write!(f, "iterator:{:?}", iterator),
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
