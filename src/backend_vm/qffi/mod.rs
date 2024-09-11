// QFFI - Quiklang Foreign Function Interface
// QFFI has core native functions, as well as extern function support.
// 1. OP_QFFI_CALL instruction run,
// 2. QFFI API invoked, with ffi registry index

pub mod native_fn;

use std::collections::HashMap;

use native_fn::{NativeFunctionEntry, NATIVE_FUNCTION_TABLE};

use crate::errors::VMRuntimeError;

use super::vm::RegisterVal;

pub type NativeFunction = fn(&[RegisterVal]) -> Result<RegisterVal, VMRuntimeError>;
pub type ExternFunction = extern "C" fn(*const RegisterVal) -> RegisterVal;

#[derive(Debug)]
pub struct QFFI {
    native_function_table: Vec<NativeFunctionEntry>,
    extern_function_table: Vec<ExternFunction>,
    extern_libraries: HashMap<String, libloading::Library>,
}

impl QFFI {
    pub fn new() -> Self {
        Self {
            native_function_table: NATIVE_FUNCTION_TABLE.clone(),
            extern_function_table: Vec::new(),
            extern_libraries: HashMap::new(),
        }
    }
}

impl Default for QFFI {
    fn default() -> Self {
        Self::new()
    }
}

impl QFFI {
    // pub fn register_native_function(&mut self, func: NativeFunction) -> usize {
    //     let index = self.native_function_table.len();
    //     self.native_function_table.push(func);
    //     index
    // }

    pub fn load_extern_function(&mut self, path: &str, name: &str) -> Result<usize, String> {
        unsafe {
            let lib = libloading::Library::new(path).map_err(|e| e.to_string())?;
            let func: libloading::Symbol<ExternFunction> =
                lib.get(name.as_bytes()).map_err(|e| e.to_string())?;
            let index = self.extern_function_table.len();
            self.extern_function_table.push(*func);
            self.extern_libraries.insert(path.to_string(), lib); // Store the library to keep it loaded
            Ok(index)
        }
    }

    pub fn call_extern_function(
        &self,
        index: usize,
        args: &[RegisterVal],
    ) -> Result<RegisterVal, VMRuntimeError> {
        if let Some(func) = self.extern_function_table.get(index) {
            let args_ptr = args.as_ptr();
            func(args_ptr)
        } else {
            Err(VMRuntimeError::UndefinedQFFIFn(index))
        }
    }

    pub fn call_native_function(
        &self,
        index: usize,
        args: &[RegisterVal],
    ) -> Result<RegisterVal, VMRuntimeError> {
        if let Some(func) = self.native_function_table.get(index) {
            (func.function)(args)
        } else {
            Err(VMRuntimeError::UndefinedNativeFn(index))
        }
    }
}
