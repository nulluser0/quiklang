use std::time::SystemTime;

use lazy_static::lazy_static;

use crate::{backend_vm::register_val::RegisterVal, errors::VMRuntimeError};

use super::NativeFunction;

#[derive(Debug, Clone)]
pub struct NativeFunctionEntry {
    pub name: &'static str,
    pub function: NativeFunction,
}

lazy_static! {
    pub(crate) static ref NATIVE_FUNCTION_TABLE: Vec<NativeFunctionEntry> = {
        // Register all native functions here
        let table = vec![
            NativeFunctionEntry { name: "println", function: println as NativeFunction },
            NativeFunctionEntry { name: "time", function: time as NativeFunction },
            NativeFunctionEntry { name: "panic", function: panic as NativeFunction },
        ];

        table
    };
}

pub(super) fn println(args: &[RegisterVal]) -> Result<RegisterVal, VMRuntimeError> {
    // Input is a string (A pointer to the string in the heap)

    // let ptr = RegisterVal {
    //     ptr: RegisterVal::set_string("what the sigma".to_string()),
    // };
    // let string = ptr.get_string()?;

    // println!("{}", string);

    // drop(ptr);

    println!(
        "{}",
        args[0].get_value_from_ptr::<String>()? // Null pointer dereference on error
    );
    Ok(RegisterVal { null: () })
}

pub(super) fn time(_args: &[RegisterVal]) -> Result<RegisterVal, VMRuntimeError> {
    let time = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(n) => n.as_secs() as i64,
        Err(e) => {
            return Err(VMRuntimeError::QFFINativeFnError(format!(
                "SystemTime before UNIX EPOCH! {}",
                e
            )))
        }
    };
    Ok(RegisterVal { int: time })
}

pub(super) fn panic(args: &[RegisterVal]) -> Result<RegisterVal, VMRuntimeError> {
    Err(VMRuntimeError::QFFINativeFnError(format!(
        "Panic: {}",
        args[0]
    )))
}
