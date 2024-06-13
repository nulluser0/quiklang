use std::{cell::RefCell, rc::Rc, time::SystemTime};

use crate::{frontend::ast::Expr, mk_integer, mk_null};

use super::{environment::Environment, values::Val};
use crate::backend::values::{IntegerVal, NullVal};

pub fn native_println(
    args: Vec<Val>,
    _env: &Rc<RefCell<Environment>>,
    _raw_exprs: Vec<Expr>,
) -> Val {
    for arg in args {
        println!("{:?}", arg); // for now, just use debug print.
                               // TODO: Add actual std::fmt::Display impl for Val enums.
    }
    mk_null!()
}

pub fn native_time(_args: Vec<Val>, _env: &Rc<RefCell<Environment>>, _raw_exprs: Vec<Expr>) -> Val {
    let time = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(n) => n.as_secs() as i64,
        Err(e) => panic!("SystemTime before UNIX EPOCH! {}", e),
    };
    mk_integer!(time)
}

pub fn native_forget(
    args: Vec<Val>,
    _env: &Rc<RefCell<Environment>>,
    _raw_exprs: Vec<Expr>,
) -> Val {
    for arg in args {
        std::mem::forget(arg)
    }
    mk_null!()
}

pub fn native_drop(_args: Vec<Val>, env: &Rc<RefCell<Environment>>, raw_exprs: Vec<Expr>) -> Val {
    for raw_expr in raw_exprs {
        if let Expr::Identifier(ident) = raw_expr {
            env.borrow_mut().drop_var(&ident);
        }
    }
    mk_null!()
}
