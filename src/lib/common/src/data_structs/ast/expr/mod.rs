//! # Expressions AST Structures
//!
//! This module contains the AST structures for expressions in Quiklang.
//!
//! [`Return to AST Module`](../index.html)
//!
//! ## List of Expression AST Structures
//! TODO: Add list of expression AST structures.

pub mod array_expr;
pub mod array_index_expr;
pub mod block_expr;
pub mod bracket_expr;
pub mod control_expr;
pub mod enum_expr;
pub mod field_access_expr;
pub mod for_expr;
pub mod func_call_expr;
pub mod if_expr;
pub mod literal_expr;
pub mod loop_expr;
pub mod match_expr;
pub mod method_call_expr;
pub mod operator_expr;
pub mod range_expr;
pub mod struct_expr;
pub mod tuple_expr;
pub mod tuple_index_expr;
pub mod underscore_expr;
pub mod while_expr;

use array_expr::{ArrayExpr, ListArrayExpr};
use array_index_expr::ArrayIndexExpr;
use block_expr::BlockExpr;
use bracket_expr::BracketExpr;
use control_expr::{BreakExpr, ContinueExpr, ReturnExpr};
use enum_expr::EnumExpr;
use field_access_expr::FieldAccessExpr;
use for_expr::ForExpr;
use func_call_expr::FuncCallExpr;
use if_expr::IfExpr;
use literal_expr::LiteralExpr;
use loop_expr::LoopExpr;
use match_expr::MatchExpr;
use method_call_expr::MethodCallExpr;
use operator_expr::OperatorExpr;
use range_expr::RangeExpr;
use struct_expr::StructExpr;
use tuple_expr::TupleExpr;
use tuple_index_expr::TupleIndexExpr;
use underscore_expr::UnderscoreExpr;
use while_expr::WhileExpr;

use super::paths::ASTExprPath;

/// Represents an expression in Quiklang.
/// All expressions are statements.
#[derive(Debug, Clone)]
pub enum Expr {
    /// An Expr Path expression.
    Path(ASTExprPath),
    /// An operation expression.
    Operator(OperatorExpr),
    /// A literal expression.
    Literal(LiteralExpr),
    /// A bracket expression.
    Bracket(BracketExpr),
    /// A function call expression.
    FuncCall(FuncCallExpr),
    /// A block expression.
    Block(BlockExpr),
    /// An if expression.
    If(IfExpr),
    /// A while expression.
    While(WhileExpr),
    /// A for expression.
    For(ForExpr),
    /// A forever loop expression.
    Loop(LoopExpr),
    /// A struct expression.
    Struct(StructExpr),
    /// An enum expression.
    Enum(EnumExpr),
    /// A match expression.
    Match(MatchExpr),
    /// A tuple expression.
    Tuple(TupleExpr),
    /// A tuple index expression.
    TupleIndex(TupleIndexExpr),
    /// An array expression.
    Array(ArrayExpr),
    /// A list array expression.
    ListArray(ListArrayExpr),
    /// An array index expression.
    ArrayIndex(ArrayIndexExpr),
    /// A method call expression.
    MethodCall(MethodCallExpr),
    /// A field access expression.
    FieldAccess(FieldAccessExpr),
    /// A range expression.
    Range(RangeExpr),
    /// A return expression.
    Return(ReturnExpr),
    /// A break expression.
    Break(BreakExpr),
    /// A continue expression.
    Continue(ContinueExpr),
    /// An underscore expression.
    Underscore(UnderscoreExpr),
}
