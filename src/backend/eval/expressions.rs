pub fn evaluate_expr(expr: Expr, env: &mut Environment) -> Val {
    match expr {
        Expr::Literal(literal) => evaluate_literal(literal),
        Expr::Identifier(identifier) => evaluate_identifier(identifier, env),
        Expr::BinaryOp { op, left, right } => evaluate_binary_op(op, *left, *right, env),
        Expr::UnaryOp(_, _) => unimplemented!(),
        Expr::FunctionCall(_, _) => unimplemented!(),
    }
}

pub fn evaluate_identifier(identifier: String, env: &mut Environment) -> Val {
    env.lookup_var(&identifier)
}

pub fn evaluate_literal(literal: Literal) -> Val {
    match literal {
        Literal::Integer(value) => mk_number!(value as f64) as Val,
        Literal::String(_) => unimplemented!(),
    }
}

pub fn evaluate_binary_op(op: BinaryOp, left: Expr, right: Expr, env: &mut Environment) -> Val {
    let left_val = evaluate_expr(left, env);
    let right_val = evaluate_expr(right, env);

    match (left_val, right_val) {
        (Val::Number(l), Val::Number(r)) => {
            let result = match op {
                BinaryOp::Add => l.value + r.value,
                BinaryOp::Subtract => l.value - r.value,
                BinaryOp::Multiply => l.value * r.value,
                BinaryOp::Divide => l.value / r.value,
                BinaryOp::Modulus => l.value % r.value,
                _ => panic!("Unsupported binary operation"),
            };
            mk_number!(result) as Val
        }
        _ => panic!("Binary operations are only supported for numbers"),
    }
}
