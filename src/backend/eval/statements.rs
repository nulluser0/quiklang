use std::{cell::RefCell, rc::Rc};

use crate::{
    backend::{
        environment::Environment,
        values::{FunctionVal, NullVal, SpecialVal, SpecialValKeyword, Val},
    },
    frontend::ast::{Expr, Stmt},
    mk_null,
};

use super::expressions::evaluate_expr;

pub fn evaluate_declare_var(
    name: String,
    is_mutable: bool,
    is_global: bool,
    expr: Option<Expr>,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Val {
    let value: Val = if let Some(val) = expr {
        evaluate_expr(val, env, root_env)
    } else {
        mk_null!()
    };

    if is_global {
        root_env.borrow_mut().declare_var(&name, value, is_mutable)
    } else {
        env.borrow_mut().declare_var(&name, value, is_mutable)
    }
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
    });

    env.borrow_mut().declare_var(&name, function, false)
}

pub fn evaluate_break_stmt(
    expr: Option<Expr>,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Val {
    match expr {
        Some(expr) => Val::Special(SpecialVal {
            keyword: SpecialValKeyword::Break,
            return_value: Some(Box::new(evaluate_expr(expr, env, root_env))),
        }),
        None => Val::Special(SpecialVal {
            keyword: SpecialValKeyword::Break,
            return_value: None,
        }),
    }
}

pub fn evaluate_return_stmt(
    expr: Option<Expr>,
    env: &Rc<RefCell<Environment>>,
    root_env: &Rc<RefCell<Environment>>,
) -> Val {
    match expr {
        Some(expr) => Val::Special(SpecialVal {
            keyword: SpecialValKeyword::Return,
            return_value: Some(Box::new(evaluate_expr(expr, env, root_env))),
        }),
        None => Val::Special(SpecialVal {
            keyword: SpecialValKeyword::Return,
            return_value: None,
        }),
    }
}
