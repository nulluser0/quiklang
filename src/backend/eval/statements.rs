use crate::{
    backend::{environment::Environment, values::Val, values::NullVal},
    frontend::ast::Expr,
    mk_null,
};

use super::expressions::evaluate_expr;

pub fn evaluate_declare_var(
    name: String,
    is_const: bool,
    is_mutable: bool,
    expr: Option<Expr>,
    env: &mut Environment,
) -> Val {
    let value: Val = if let Some(val) = expr {
        evaluate_expr(val, env)
    } else {
        mk_null!()
    };
    
    env.declare_var(&name, value)
}
