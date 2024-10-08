// RegisterVal, and alloc

use std::{fmt::Debug, hash::Hash, sync::Arc};

use crate::errors::VMRuntimeError;

use super::bytecode_compiler::compiler::TaggedConstantValue;

#[repr(C, align(8))]
#[derive(Clone, Copy)]
pub union RegisterVal {
    pub null: (), // Nulls, ints, floats, and bools are all primitives, so they are stored directly in the union.
    pub int: i64,
    pub float: f64,
    pub bool: bool,
    pub ptr: *const (), // Universal pointer to heap allocated object. Unsafe, so requires a destructor and a brain to manage memory.
}

impl Default for RegisterVal {
    fn default() -> Self {
        RegisterVal { null: () }
    }
}

// Implement Send and Sync for RegisterVal
unsafe impl Send for RegisterVal {}
unsafe impl Sync for RegisterVal {}

impl PartialEq for RegisterVal {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.int == other.int }
    }
}

impl Eq for RegisterVal {}

impl Hash for RegisterVal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe { self.int.hash(state) }
    }
}

impl Debug for RegisterVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Access the union as its raw bytes
        unsafe { self.int.fmt(f) }
    }
}

impl std::fmt::Display for RegisterVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // match self {
        //     RegisterVal::Null => write!(f, "null"),
        //     RegisterVal::Int(int) => write!(f, "{}", int),
        //     RegisterVal::Float(float) => write!(f, "{}", float),
        //     RegisterVal::Bool(boolean) => write!(f, "{}", boolean),
        //     RegisterVal::Str(string) => write!(f, "{}", string),
        //     RegisterVal::Array(array) => write!(f, "{:?}", array),
        //     RegisterVal::Range(range) => write!(f, "{:?}", range),
        //     RegisterVal::HashMap(hashmap) => write!(f, "{:?}", hashmap),
        //     RegisterVal::HashSet(hashset) => write!(f, "{:?}", hashset),
        // }

        unsafe { write!(f, "{:?}", self.int) }
    }
}

impl RegisterVal {
    /// Drop the value stored in the pointer. The pointer is then set to null.
    /// Safety: The caller must ensure that the pointer is not null.
    pub fn drop_ptr<T>(&self) {
        let ptr = unsafe { self.ptr } as *mut T;
        if ptr.is_null() {
            return;
        }
        unsafe {
            // let _ = Arc::from_raw(ptr); // Drop the value
            Arc::decrement_strong_count(ptr as *const T);
        };
    }

    /// Returns the pointer of a heap allocated object (T).
    pub fn set_ptr_from_value<T>(value: T) -> *const () {
        let ptr = Arc::into_raw(Arc::new(value));
        ptr as *const ()
    }

    /// Returns the value of a heap allocated object (T) from its pointer.
    pub fn get_value_from_ptr<T>(&self) -> Result<&T, VMRuntimeError> {
        let ptr = unsafe { self.ptr } as *const T;
        unsafe { ptr.as_ref().ok_or(VMRuntimeError::NullPtrDeref) }
    }

    /// Increments the reference count of the heap allocated object.
    pub fn inc_arc_ptr<T>(&self) {
        let ptr = unsafe { self.ptr } as *mut T;
        if ptr.is_null() {
            return;
        }
        unsafe {
            Arc::increment_strong_count(ptr as *const T);
        }
    }

    /// Decrements the reference count of the heap allocated object.
    pub fn dec_arc_ptr<T>(&self) {
        let ptr = unsafe { self.ptr } as *mut T;
        if ptr.is_null() {
            return;
        }
        unsafe {
            Arc::decrement_strong_count(ptr as *const T);
        }
    }

    /// Equivalent to `Arc::clone` but for a raw pointer.
    pub fn set_ptr_from_ref(&self) -> Result<*const (), VMRuntimeError> {
        // Get the pointer from the union
        let ptr = unsafe { self.ptr };

        // Check if the pointer is null
        if ptr.is_null() {
            return Err(VMRuntimeError::NullPtrDeref);
        }

        // Convert the raw pointer back to an Arc
        // Safety: The caller must ensure that this pointer was created by set_ptr_from_value
        let arc = unsafe { Arc::from_raw(ptr as *mut ()) };

        // Clone the Arc, which will increment the reference count
        let new_ptr = Arc::into_raw(arc.clone());

        // Return the new raw pointer
        Ok(new_ptr)
    }

    // pub fn get_string(&self) -> Result<&String, VMRuntimeError> {
    //     let ptr = unsafe { self.ptr } as *mut String;
    //     unsafe { ptr.as_ref().ok_or(VMRuntimeError::NullPtrDeref) }
    // }
    //
    // pub fn set_string(string: String) -> *const () {
    //     let ptr = Box::into_raw(Box::new(string));
    //     ptr as *const ()
    // }
    //
    // pub fn get_array(&self) -> Result<&Vec<RegisterVal>, VMRuntimeError> {
    //     let ptr = unsafe { self.ptr } as *mut Vec<RegisterVal>;
    //     unsafe { ptr.as_ref().ok_or(VMRuntimeError::NullPtrDeref) }
    // }
    //
    // pub fn set_array(array: Vec<RegisterVal>) -> *const () {
    //     let ptr = Box::into_raw(Box::new(array));
    //     ptr as *const ()
    // }
    //
    // pub fn get_hashmap(&self) -> &HashMap<RegisterVal, RegisterVal> {
    //     let ptr = unsafe { self.ptr } as *mut HashMap<RegisterVal, RegisterVal>;
    //     unsafe { &*ptr }
    // }
    //
    // pub fn set_hashmap(hashmap: HashMap<RegisterVal, RegisterVal>) -> *const () {
    //     let ptr = Box::into_raw(Box::new(hashmap));
    //     ptr as *const ()
    // }
    //
    // pub fn get_hashset(&self) -> &HashSet<RegisterVal> {
    //     let ptr = unsafe { self.ptr } as *mut HashSet<RegisterVal>;
    //     unsafe { &*ptr }
    // }
    //
    // pub fn set_hashset(hashset: HashSet<RegisterVal>) -> *const () {
    //     let ptr = Box::into_raw(Box::new(hashset));
    //     ptr as *const ()
    // }
}

pub fn to_quiklangc_strings(inner: &TaggedConstantValue) -> String {
    match inner {
        TaggedConstantValue::Null => format!("{:10}| null", "null"),
        TaggedConstantValue::Int(int) => format!("{:10}| {}", "integer", int),
        TaggedConstantValue::Float(float) => format!("{:10}| {}", "float", float),
        TaggedConstantValue::Bool(boolean) => format!("{:10}| {}", "bool", boolean),
        TaggedConstantValue::Str(string) => format!("{:10}| {}", "string", string),
    }

    // unsafe {
    //     match inner {
    //         RegisterVal { null: () } => format!("{:10}| null", "null"),
    //         RegisterVal { int } => format!("{:10}| {}", "integer", int),
    //         RegisterVal { float } => format!("{:10}| {}", "float", float),
    //         RegisterVal { bool } => format!("{:10}| {}", "bool", bool),
    //         RegisterVal { ptr } => format!("{:10}| {:?}", "pointer", ptr),
    //     }
    // }
}
