use std::{cell::RefCell, rc::Rc};

use crate::{
    backend::{
        environment::Environment,
        values::{FunctionVal, NullVal, Val},
    },
    frontend::ast::{Expr, Stmt},
    mk_null,
};

use super::expressions::evaluate_expr;

pub fn evaluate_declare_var(
    name: String,
    is_mutable: bool,
    expr: Option<Expr>,
    env: &Rc<RefCell<Environment>>,
) -> Val {
    let value: Val = if let Some(val) = expr {
        evaluate_expr(val, env)
    } else {
        mk_null!()
    };

    env.borrow_mut().declare_var(&name, value, is_mutable)
}

pub fn evaluate_declare_fn(
    parameters: Vec<String>,
    name: String,
    body: Vec<Stmt>,
    is_async: bool,
    env: &Rc<RefCell<Environment>>,
) -> Val {
    let function: Val = Val::Function(FunctionVal {
        name: name.clone(),
        parameters,
        body,
        is_async,
        declaration_env: env.clone(),
    });

    env.borrow_mut().declare_var(&name, function, false)
}
