//! # Operator Expressions AST Structures
//!
//! This module contains the AST structures for operator expressions in Quiklang.
//!
//! [`Return to Expr Module`](../index.html)
//!
//! ## List of Operator Expression AST Structures
//! - [`OperatorExpr`](#operator-expr)
//! - [`BinaryOperatorExpr`](#binary-operator-expr)
//! - [`BinaryOperator`](#binary-operator)
//! - [`UnaryOperatorExpr`](#unary-operator-expr)
//! - [`UnaryOperator`](#unary-operator)
//! - [`ComparisonOperatorExpr`](#comparison-operator-expr)
//! - [`ComparisonOperator`](#comparison-operator)
//! - [`BooleanOperatorExpr`](#boolean-operator-expr)
//! - [`BooleanOperator`](#boolean-operator)
//! - [`AssignmentOperatorExpr`](#assignment-operator-expr)
//! - [`BinaryAssignmentOperatorExpr`](#binary-assignment-operator-expr)
//! - [`TypeCastOperatorExpr`](#type-cast-operator-expr)

use super::Expr;

/// Operator expression.
#[derive(Debug, Clone)]
pub enum OperatorExpr {
    /// Binary operator expression.
    Binary(BinaryOperatorExpr),
    /// Unary operator expression.
    Unary(UnaryOperatorExpr),
    /// Comparison operator expression.
    Comparison(ComparisonOperatorExpr),
    /// Boolean operator expression.
    Boolean(BooleanOperatorExpr),
    /// Assignment operator expression.
    Assignment(AssignmentOperatorExpr),
    /// Binary assignment operator expression.
    BinaryAssignment(BinaryAssignmentOperatorExpr),
    /// Type cast operator expression.
    TypeCast(TypeCastOperatorExpr),
}

/// Binary operator expression.
/// Represents a binary operation.
/// Example: `x + y`, `x * y`.
/// List of binary operators:
/// - `+` Addition
/// - `-` Subtraction
/// - `*` Multiplication
/// - `/` Division
/// - `%` Modulus
/// - `&` Bitwise AND
/// - `|` Bitwise OR
/// - `^` Bitwise XOR
/// - `<<` Bitwise left shift
/// - `>>` Bitwise right shift
#[derive(Debug, Clone)]
pub struct BinaryOperatorExpr {
    /// The left-hand side expression.
    pub lhs: Box<Expr>,
    /// The binary operator.
    pub op: BinaryOperator,
    /// The right-hand side expression.
    pub rhs: Box<Expr>,
}

/// Binary operator.
/// Represents a binary operator.
/// Example: `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `<<`, `>>`.
#[derive(Debug, Clone)]
pub enum BinaryOperator {
    /// Addition operator. `+`
    Add,
    /// Subtraction operator. `-`
    Sub,
    /// Multiplication operator. `*`
    Mul,
    /// Division operator. `/`
    Div,
    /// Modulus operator. `%`
    Mod,
    /// Bitwise AND operator. `&`
    BitAnd,
    /// Bitwise OR operator. `|`
    BitOr,
    /// Bitwise XOR operator. `^`
    BitXor,
    /// Bitwise left shift operator. `<<`
    BitLeftShift,
    /// Bitwise right shift operator. `>>`
    BitRightShift,
}

/// Unary operator expression.
/// Represents a unary operation.
/// Example: `-x`, `!x`.
/// List of unary operators:
/// - `-` Negation
/// - `!` Logical NOT
/// - `~` Bitwise NOT
/// - `*` Dereference
/// - `ref` Reference
/// - `mut ref` Mutable reference
#[derive(Debug, Clone)]
pub struct UnaryOperatorExpr {
    /// The unary operator.
    pub op: UnaryOperator,
    /// The expression.
    pub expr: Box<Expr>,
}

/// Unary operator.
/// Represents a unary operator.
/// Example: `-`, `!`, `~`, `*`, `ref`, `mut ref`.
#[derive(Debug, Clone)]
pub enum UnaryOperator {
    /// Negation operator. `-`
    Neg,
    /// Logical NOT operator. `!`
    Not,
    /// Bitwise NOT operator. `~`
    BitNot,
    /// Dereference operator. `*`
    Deref,
    /// Reference operator. `ref`
    Ref,
    /// Mutable reference operator. `mut ref`
    MutRef,
}

/// Comparison operator expression.
/// Represents a comparison operation.
/// Example: `x == y`, `x != y`, `x < y`, `x > y`, `x <= y`, `x >= y`.
/// List of comparison operators:
/// - `==` Equal
/// - `!=` Not equal
/// - `<` Less than
/// - `>` Greater than
/// - `<=` Less than or equal
/// - `>=` Greater than or equal
#[derive(Debug, Clone)]
pub struct ComparisonOperatorExpr {
    /// The left-hand side expression.
    pub lhs: Box<Expr>,
    /// The comparison operator.
    pub op: ComparisonOperator,
    /// The right-hand side expression.
    pub rhs: Box<Expr>,
}

/// Comparison operator.
/// Represents a comparison operator.
/// Example: `==`, `!=`, `<`, `>`, `<=`, `>=`.
#[derive(Debug, Clone)]
pub enum ComparisonOperator {
    /// Equal operator. `==`
    Equal,
    /// Not equal operator. `!=`
    NotEqual,
    /// Less than operator. `<`
    LessThan,
    /// Greater than operator. `>`
    GreaterThan,
    /// Less than or equal operator. `<=`
    LessThanOrEqual,
    /// Greater than or equal operator. `>=`
    GreaterThanOrEqual,
}

/// Boolean operator expression.
/// Represents a boolean operation.
/// Example: `x && y`, `x || y`.
/// List of boolean operators:
/// - `&&` Logical AND
/// - `||` Logical OR
#[derive(Debug, Clone)]
pub struct BooleanOperatorExpr {
    /// The left-hand side expression.
    pub lhs: Box<Expr>,
    /// The boolean operator.
    pub op: BooleanOperator,
    /// The right-hand side expression.
    pub rhs: Box<Expr>,
}

/// Boolean operator.
/// Represents a boolean operator.
/// Example: `&&`, `||`.
#[derive(Debug, Clone)]
pub enum BooleanOperator {
    /// Logical AND operator. `&&`
    And,
    /// Logical OR operator. `||`
    Or,
}

/// Assignment operator expression.
/// Represents an assignment operation.
/// Example: `x = y`.
/// List of assignment operators:
/// - `=` Assignment
#[derive(Debug, Clone)]
pub struct AssignmentOperatorExpr {
    /// The left-hand side expression.
    pub lhs: Box<Expr>,
    /// The right-hand side expression.
    pub rhs: Box<Expr>,
}

/// Binary assignment operator expression.
/// Represents a binary assignment operation.
/// Example: `x += y`, `x -= y`, `x *= y`, `x /= y`, `x %= y`, `x &= y`, `x |= y`, `x ^= y`, `x <<= y`, `x >>= y`.
/// List of binary assignment operators:
/// - `+=` Addition assignment
/// - `-=` Subtraction assignment
/// - `*=` Multiplication assignment
/// - `/=` Division assignment
/// - `%=` Modulus assignment
/// - `&=` Bitwise AND assignment
/// - `|=` Bitwise OR assignment
/// - `^=` Bitwise XOR assignment
/// - `<<=` Bitwise left shift assignment
/// - `>>=` Bitwise right shift assignment
#[derive(Debug, Clone)]
pub struct BinaryAssignmentOperatorExpr {
    /// The left-hand side expression.
    pub lhs: Box<Expr>,
    /// The binary assignment operator, which is also a binary operator.
    pub op: BinaryOperator,
    /// The right-hand side expression.
    pub rhs: Box<Expr>,
}

/// Type cast operator expression.
/// Represents a type cast operation.
/// Example: `x as i32`.
/// List of type cast operators:
/// - `as` Type cast
#[derive(Debug, Clone)]
pub struct TypeCastOperatorExpr {
    /// The expression to cast.
    pub expr: Box<Expr>,
    /// The type to cast to.
    pub ty: Type,
}
