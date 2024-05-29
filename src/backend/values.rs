#[derive(Debug, PartialEq)]
pub enum ValueType {
    Null,
    Number,
    Bool,
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
pub struct NumberVal {
    pub value: f64,
}

impl RuntimeVal for NumberVal {
    fn get_type(&self) -> ValueType {
        ValueType::Number
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

// This enum encapsulates any RuntimeVal type to handle them generically.
#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    Null(NullVal),
    Number(NumberVal),
    Bool(BoolVal),
}

impl RuntimeVal for Val {
    fn get_type(&self) -> ValueType {
        match self {
            Val::Null(_) => ValueType::Null,
            Val::Number(_) => ValueType::Number,
            Val::Bool(_) => ValueType::Bool,
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
macro_rules! mk_number {
    ($n:expr) => {
        Val::Number(NumberVal { value: $n })
    };
    () => {
        mk_number!(0.0)
    };
}
