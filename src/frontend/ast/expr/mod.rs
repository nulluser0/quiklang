//! # Expressions AST Structures
//!
//! This module contains the AST structures for expressions in Quiklang.
//!
//! [`Return to AST Module`](../index.html)
//!
//! ## List of Expression AST Structures
//! TODO: Add list of expression AST structures.

pub mod block;
pub mod bracket;
pub mod funccall;
pub mod identpath;
pub mod literal;
pub mod operator;

use block::BlockExpr;
use bracket::BracketExpr;
use funccall::FuncCallExpr;
use identpath::IdentPathExpr;
use literal::LiteralExpr;
use operator::OperatorExpr;

use super::stmt::Stmt;

/// Represents an expression in Quiklang.
/// All expressions are statements.
#[derive(Debug, Clone)]
pub enum Expr {
    /// An operation expression.
    Operator(OperatorExpr),
    /// An identifier path expression.
    IdentPath(IdentPathExpr),
    /// A literal expression.
    Literal(LiteralExpr),
    /// A bracket expression.
    Bracket(BracketExpr),
    /// A function call expression.
    FuncCall(FuncCallExpr),
    /// A block expression.
    Block(BlockExpr),
    /// An if expression.
    If(IfExpr), // TODO: Continue refactoring beginning from here.
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

/// If expression.
/// Represents an if expression.
/// Example: `if x < y { println("x is less than y") } else { println("x is greater than or equal to y") }`.
/// The condition is an expression that evaluates to a boolean.
/// The then block is executed if the condition is true.
/// The else block is executed if the condition is false.
/// The else block is optional.
/// The if expression returns a value if both the then and else blocks return a value. Else, it returns `null`.
/// Quiklang will complain if it else block is missing when a value is returned from the then block.
#[derive(Debug, Clone)]
pub struct IfExpr {
    /// The condition to check.
    pub cond: Box<Expr>,
    /// The then block to execute if the condition is true.
    pub then_block: Box<Expr>,
    /// The else block to execute if the condition is false.
    pub else_block: Option<Box<Expr>>,
}

/// While expression.
/// Represents a while loop.
/// Example: `while x < 10 { println(x as string); x += 1 }`.
/// The condition is an expression that evaluates to a boolean.
#[derive(Debug, Clone)]
pub struct WhileExpr {
    /// The condition to check.
    pub cond: Box<Expr>,
    /// The block to execute while the condition is true.
    pub block: Box<Expr>,
}

/// For expression.
/// Represents a for loop.
/// Example: `for x in 0..10 { println(x) }`, `for x in [1, 2, 3] { println(x as string) }`.
/// The for loop iterable needs to implement the `IntoIterator` trait.
#[derive(Debug, Clone)]
pub struct ForExpr {
    /// The variable name.
    pub name: &'static str,
    /// The iterable to loop over.
    pub iterable: Box<Expr>,
    /// The block to execute for each item in the iterable.
    pub block: Box<Expr>,
}

/// Forever loop expression.
/// Represents a forever loop.
/// Example: `loop { println("Hello, World!") }`.
/// The loop block will be executed forever, until a `break` or `return` expression is encountered.
#[derive(Debug, Clone)]
pub struct LoopExpr {
    /// The block to execute forever.
    pub block: Box<Expr>,
}

/// Struct expression.
/// Represents an instance of a struct being created.
/// Example: `Point { x: 1, y: 2 }`.
#[derive(Debug, Clone)]
pub struct StructExpr {
    /// The name of the struct.
    pub name: &'static str,
    /// The fields of the struct.
    pub fields: Vec<FilledStructField>,
}

/// Filled struct field.
/// Represents a field in a struct that is filled with a value.
#[derive(Debug, Clone)]
pub struct FilledStructField {
    /// The name of the field.
    pub name: &'static str,
    /// The value of the field.
    pub value: Expr,
}

/// Enum expression.
/// Represents an instance of an enum variant being created.
/// Example: `Variant::Value`, `Variant::Tuple(42, 3.14)`, `Variant::Struct { x: 1, y: 2 }`.
#[derive(Debug, Clone)]
pub struct EnumExpr {
    /// The name of the enum.
    pub name: &'static str,
    /// The variant of the enum.
    pub variant: FilledEnumVariant,
}

/// Filled enum variant.
/// Represents a variant in an enum that is filled with a value.
#[derive(Debug, Clone)]
pub struct FilledEnumVariant {
    /// The name of the variant.
    pub name: &'static str,
    /// The value of the variant.
    pub value: Option<FilledEnumField>,
}

/// Filled enum field.
/// Represents a field in an enum variant that is filled with a value.
#[derive(Debug, Clone)]
pub enum FilledEnumField {
    /// No fields.
    None,
    /// Tuple fields.
    Tuple(Vec<Expr>),
    /// Struct fields.
    Struct(Vec<FilledStructField>),
}

/// Match expression.
/// Represents a match expression.
/// Example: `match x { 0 => println("Zero"), 1 => println("One"), _ => println("Other") }`.
/// Useful for pattern matching and destructuring.
#[derive(Debug, Clone)]
pub struct MatchExpr {
    /// The expression to match.
    pub expr: Box<Expr>,
    /// The match arms.
    pub arms: Vec<MatchArm>,
}

/// Match arm.
/// Represents a match arm in a match expression.
/// Example: `0 => println("Zero")`.
#[derive(Debug, Clone)]
pub struct MatchArm {
    /// The pattern to match.
    pub pattern: MatchPattern,
    /// The expression to execute
    pub expr: Box<Expr>,
}

/// Tuple expression.
/// Represents a tuple.
/// Example: `(42, 3.14, "Hello, World!")`.
/// A tuple is a fixed-size collection of values.
/// The values can be of different types.
#[derive(Debug, Clone)]
pub struct TupleExpr {
    /// The values in the tuple.
    pub values: Vec<Expr>,
}

/// Tuple index expression.
/// Represents an index into a tuple.
/// Example: `tuple.0`, `tuple.1`, `tuple.2`.
/// The index is zero-based.
/// The index must be a constant integer.
/// The index must be in bounds.
/// The type of the expression is the type of the value at the index.
/// Compiler error if the index is out of bounds.
#[derive(Debug, Clone)]
pub struct TupleIndexExpr {
    /// The tuple expression.
    pub tuple: Box<Expr>,
    /// The index into the tuple.
    pub index: usize,
}

/// Array expression.
/// Represents an array.
/// Example: `[1, 2, 3, 4, 5]`.
/// Arrays are dynamically sized collections of values.
/// The compiler however can infer if the array is a fixed-size array.
#[derive(Debug, Clone)]
pub struct ArrayExpr {
    /// The values in the array.
    pub values: Vec<Expr>,
    /// The size of the array.
    pub size: Option<usize>,
}

/// Array index expression.
/// Represents an index into an array.
/// Example: `array[0]`, `array[1]`, `array[2]`, `hashmap["key"]`.
/// The index is zero-based.
/// The index must be a constant integer.
/// If the array is fixed-size, the index must be in bounds.
/// Otherwise, index out of bounds is a runtime error.
#[derive(Debug, Clone)]
pub struct ArrayIndexExpr {
    /// The array expression.
    pub array: Box<Expr>,
    /// The index into the array.
    pub index: Box<Expr>,
}

/// Method call expression.
/// Represents a method call.
/// Example: `string.len()`, `array.push(42)`.
/// The method is called on a struct/enum with a list of arguments.
/// The first argument is the struct/enum itself, automatically passed by dot syntax.
#[derive(Debug, Clone)]
pub struct MethodCallExpr {
    /// The method to call.
    pub method: Box<Expr>,
    /// The arguments to the method.
    pub args: Vec<Expr>,
}

/// Field access expression.
/// Represents an access to a field in a struct or enum.
/// Example: `point.x`, `point.y`, `variant.field`.
/// The field must exist in the struct or enum.
/// The type of the expression is the type of the field.
/// Compiler error if the field does not exist.
/// Compiler error if the field is private.
#[derive(Debug, Clone)]
pub struct FieldAccessExpr {
    /// The struct/enum expression.
    pub expr: Box<Expr>,
    /// The name of the field.
    pub field: &'static str,
}

/// Range expression.
/// Represents a range.
/// Example: `0..10`, `0..=10`.
/// The range is inclusive on the start and exclusive on the end.
/// When the range is inclusive on the end, it is written as `..=`.
/// Implement the `IntoIterator` trait to use the range in a for loop.
#[derive(Debug, Clone)]
pub struct RangeExpr {
    /// The start of the range.
    pub start: Box<Expr>,
    /// The end of the range.
    pub end: Box<Expr>,
    /// Is the range inclusive on the end?
    pub inclusive: bool,
}

/// Return expression.
/// Represents a return statement.
/// Example: `return 42`, `return "Hello, World!"`.
/// The return value must match the return type of the function.
/// The return value is optional.
/// If the return value is missing, the return type must be `null`.
/// The return value is the last expression in the block.
/// Compiler error if the return value mismatches the return type.
#[derive(Debug, Clone)]
pub struct ReturnExpr {
    /// The return value.
    pub value: Option<Box<Expr>>,
}

/// Break expression.
/// Represents a break statement.
/// Example: `break`, `break 42`, `break "Hello, World!"`.
/// The break value must match the loop type.
/// The break value is optional.
/// If the break value is missing, the loop type must be `null`.
/// Compiler error if the break value mismatches the loop type.
#[derive(Debug, Clone)]
pub struct BreakExpr {
    /// The break value.
    pub value: Option<Box<Expr>>,
}

/// Continue expression.
/// Represents a continue statement.
/// Example: `continue`.
/// The continue statement is used to skip the rest of the loop body and start the next iteration.
/// The continue statement is only allowed in loop bodies.
#[derive(Debug, Clone)]
pub struct ContinueExpr {}

/// Underscore expression.
/// Represents an underscore.
/// Example: `_`, `let _ = 42`.
/// The underscore is used to ignore a value.
/// The underscore is used to ignore a variable binding.
#[derive(Debug, Clone)]
pub struct UnderscoreExpr {}
