// RegisterVal, and alloc

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::errors::VMRuntimeError;

use super::bytecode_compiler::compiler::TaggedConstantValue;

// TODO: Consider using this:
//      Replace Rc<T> with *const T (raw pointer) for performance reasons.
//      As a result, carefully manage memory in the parsetime and compiletime.
//      In Parsetime, variables are dropped after their scope is dropped.
//      Example
//      block {
//          let x = [20]; // x is a pointer to a heap array.
//          block {
//              x.push(23); // x, an array, has its push method called. It modifies the heap array by accessing pointer.
//          }
//          // more stuff with x
//      } // x, a pointer to heap allocated array value, is dropped. It should be done by a "destructor" insstruction, which reads register's
//      //   pointer to the heap object, then destroys the heap object.
//      x.pop() // Parse error, x no longer exists (dropped) and cannot be accessed.
//
//      Issues. What about heap allocated objects in a heap allocated object (like an array)? How do we know all the references to that array don't exist?
//          Very temporary (bandaid-ahh) solution: pointers and concept of shared variables are not implemented. Theoretically, it should be fine to just
//          drop the "popped" object in the parent object.
//      When concepts of pointers/references/shared variables comes up, a solution is to ensure:
//          let A = B; where A is pointer/reference/share of heap object B (B would be represented as a heap pointer in vm runtime btw).
//          B must not be dropped/out of scope before A
//              let b = [123];
//                  block {
//                      let a = share b; // Valid, b does not go out of scope when a is alive.
//                  }

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
    pub fn drop_value(&self) {
        let ptr = unsafe { self.ptr } as *mut ();
        if ptr.is_null() {
            return;
        }
        unsafe {
            let _ = Box::from_raw(ptr); // Drop the value
        };
    }

    pub fn get_string(&self) -> Result<&String, VMRuntimeError> {
        let ptr = unsafe { self.ptr } as *mut String;
        unsafe { ptr.as_ref().ok_or(VMRuntimeError::NullPtrDeref) }
    }

    pub fn set_string(string: String) -> *const () {
        let ptr = Box::into_raw(Box::new(string));
        ptr as *const ()
    }

    pub fn get_array(&self) -> Result<&Vec<RegisterVal>, VMRuntimeError> {
        let ptr = unsafe { self.ptr } as *mut Vec<RegisterVal>;
        unsafe { ptr.as_ref().ok_or(VMRuntimeError::NullPtrDeref) }
    }

    pub fn set_array(array: Vec<RegisterVal>) -> *const () {
        let ptr = Box::into_raw(Box::new(array));
        ptr as *const ()
    }

    pub fn get_hashmap(&self) -> &HashMap<RegisterVal, RegisterVal> {
        let ptr = unsafe { self.ptr } as *mut HashMap<RegisterVal, RegisterVal>;
        unsafe { &*ptr }
    }

    pub fn set_hashmap(hashmap: HashMap<RegisterVal, RegisterVal>) -> *const () {
        let ptr = Box::into_raw(Box::new(hashmap));
        ptr as *const ()
    }

    pub fn get_hashset(&self) -> &HashSet<RegisterVal> {
        let ptr = unsafe { self.ptr } as *mut HashSet<RegisterVal>;
        unsafe { &*ptr }
    }

    pub fn set_hashset(hashset: HashSet<RegisterVal>) -> *const () {
        let ptr = Box::into_raw(Box::new(hashset));
        ptr as *const ()
    }
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
