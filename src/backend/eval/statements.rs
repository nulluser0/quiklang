use crate::{
    backend::{environment::Environment, values::Val},
    frontend::ast::Expr,
};

pub fn evaluate_declare_var(
    name: String,
    is_const: bool,
    is_mutable: bool,
    expr: Option<Expr>,
    env: &mut Environment,
) -> Val {
    todo!()
}
