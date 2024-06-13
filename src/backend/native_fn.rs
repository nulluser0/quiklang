use std::{cell::RefCell, rc::Rc, time::SystemTime};

use crate::{backend::eval::expressions::evaluate_expr, frontend::ast::Expr, mk_integer, mk_null};

use super::{environment::Environment, values::Val};
use crate::backend::values::{IntegerVal, NullVal};

pub fn native_println(args: Vec<Expr>, env: &Rc<RefCell<Environment>>) -> Val {
    let evaluated_args: Vec<Val> = args
        .into_iter()
        .map(|expr| evaluate_expr(expr, env))
        .collect();
    for arg in evaluated_args {
        println!("{:?}", arg); // for now, just use debug print.
                               // TODO: Add actual std::fmt::Display impl for Val enums.
    }
    mk_null!()
}

pub fn native_time(_args: Vec<Expr>, _env: &Rc<RefCell<Environment>>) -> Val {
    let time = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(n) => n.as_secs() as i64,
        Err(e) => panic!("SystemTime before UNIX EPOCH! {}", e),
    };
    mk_integer!(time)
}

pub fn native_forget(args: Vec<Expr>, _env: &Rc<RefCell<Environment>>) -> Val {
    for arg in args {
        std::mem::forget(arg)
    }
    mk_null!()
}

pub fn native_drop(args: Vec<Expr>, env: &Rc<RefCell<Environment>>) -> Val {
    for raw_expr in args {
        if let Expr::Identifier(ident) = raw_expr {
            Environment::drop_var(env, &ident);
        }
    }
    mk_null!()
}
