use std::{cell::RefCell, rc::Rc, time::SystemTime};

use crate::{
    backend_interpreter::eval::expressions::evaluate_expr, errors::InterpreterError,
    frontend::ast::Expr, mk_integer, mk_null,
};

use super::{environment::Environment, values::Val};
use crate::backend_interpreter::values::{IntegerVal, NullVal};

pub fn native_println(
    args: Vec<(Expr, bool)>,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let evaluated_args: Result<Vec<Val>, InterpreterError> = args
        .into_iter()
        .map(|expr| evaluate_expr(expr.0, env, root_env))
        .collect();
    let evaluated_args: Vec<Val> = evaluated_args?;

    for arg in evaluated_args {
        print!("{}", arg);
    }
    println!();
    Ok(mk_null!())
}

pub fn native_time(
    _args: Vec<(Expr, bool)>,
    _env: &Rc<RefCell<Environment>>,
    _root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    let time = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(n) => n.as_secs() as i64,
        Err(e) => panic!("SystemTime before UNIX EPOCH! {}", e),
    };
    Ok(mk_integer!(time))
}

pub fn native_forget(
    args: Vec<(Expr, bool)>,
    _env: &Rc<RefCell<Environment>>,
    _root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    for arg in args {
        std::mem::forget(arg)
    }
    Ok(mk_null!())
}

pub fn native_drop(
    args: Vec<(Expr, bool)>,
    env: &Rc<RefCell<Environment>>,
    _root_env: &Rc<RefCell<Environment>>,
) -> Result<Val, InterpreterError> {
    for raw_expr in args {
        if let Expr::Identifier(ident) = raw_expr.0 {
            Environment::drop_var(env, &ident);
        }
    }
    Ok(mk_null!())
}
