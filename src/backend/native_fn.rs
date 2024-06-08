use std::{cell::RefCell, rc::Rc, time::SystemTime};

use crate::{mk_integer, mk_null};

use super::{environment::Environment, values::Val};
use crate::backend::values::{IntegerVal, NullVal};

pub fn native_println(args: Vec<Val>, _env: &Rc<RefCell<Environment>>) -> Val {
    for arg in args {
        println!("{:?}", arg); // for now, just use debug print.
                               // TODO: Add actual std::fmt::Display impl for Val enums.
    }
    mk_null!()
}

pub fn native_time(_args: Vec<Val>, _env: &Rc<RefCell<Environment>>) -> Val {
    let time = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(n) => n.as_secs() as i64,
        Err(e) => panic!("SystemTime before UNIX EPOCH! {}", e),
    };
    mk_integer!(time)
}
