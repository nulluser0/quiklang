use std::time::SystemTime;

use lazy_static::lazy_static;

use crate::backend_vm::vm::RegisterVal;

use super::NativeFunction;

lazy_static! {
    pub(super) static ref NATIVE_FUNCTION_TABLE: Vec<NativeFunction> = {
        // Register all native functions here
        let table = vec![
            println as NativeFunction,
            time as NativeFunction,
        ];

        table
    };
}

pub(super) fn println(args: &[RegisterVal]) -> Result<RegisterVal, String> {
    println!("{}", args[0]);
    Ok(RegisterVal::Null)
}

pub(super) fn time(_args: &[RegisterVal]) -> Result<RegisterVal, String> {
    let time = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(n) => n.as_secs() as i64,
        Err(e) => panic!("SystemTime before UNIX EPOCH! {}", e),
    };
    Ok(RegisterVal::Int(time))
}
