// AST logic

use super::lexer::Operator;

// Program struct
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Stmt>, // SHOULD BE STMT!!!
}

impl Program {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Program { statements }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(Literal),
    Array(Vec<Expr>),
    Identifier(String),
    AssignmentExpr {
        assignee: Box<Expr>,
        expr: Box<Expr>,
    }, // Assignee, Expr
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp(UnaryOp, Box<Expr>),
    FunctionCall(Vec<Expr>, Box<Expr>), // Args, Caller
    Member(Box<Expr>, Box<Expr>, bool), // Object, Property, Computed? (a[b] = true), (a.b = false)
    IfExpr {
        condition: Box<Expr>,
        then: Vec<Stmt>,
        else_stmt: Option<Vec<Stmt>>,
    }, // Condition, Then, Optional Else
    ForExpr {
        item: Box<Expr>,
        then: Vec<Stmt>,
    }, // For item in iterable, Do Stmts
    WhileExpr {
        condition: Box<Expr>,
        then: Vec<Stmt>,
    }, // While Condition, Do Stmts
    ForeverLoopExpr(Vec<Stmt>),         // Forever loop! Has vec of stmts.
    SpecialNull, // Literally just returns null. Should ONLY be returned as a result of a semicolon.
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Identifier(keyword) => write!(f, "{:?}", keyword),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    ExprStmt(Expr),
    DeclareStmt {
        name: String,
        is_mutable: bool,
        is_global: bool,
        expr: Option<Expr>,
    },
    ReturnStmt(Option<Expr>),
    BreakStmt(Option<Expr>),
    FunctionDeclaration {
        parameters: Vec<String>,
        name: String,
        body: Vec<Stmt>,
        is_async: bool,
    }, // Parameters, Name, Body, Is async?
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Object(Vec<Property>),
    // Other literal types...
}

#[derive(Debug, PartialEq, Clone)]
pub struct Property {
    pub key: String,
    pub value: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    GreaterOrEqual,
    LessOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
    Modulus,
}

impl From<Operator> for BinaryOp {
    fn from(op: Operator) -> Self {
        match op {
            Operator::Add => BinaryOp::Add,
            Operator::Subtract => BinaryOp::Subtract,
            Operator::Multiply => BinaryOp::Multiply,
            Operator::Divide => BinaryOp::Divide,
            Operator::GreaterThan => BinaryOp::GreaterThan,
            Operator::LessThan => BinaryOp::LessThan,
            Operator::GreaterOrEqual => BinaryOp::GreaterOrEqual,
            Operator::LessOrEqual => BinaryOp::LessOrEqual,
            Operator::Equal => BinaryOp::Equal,
            Operator::NotEqual => BinaryOp::NotEqual,
            Operator::And => BinaryOp::And,
            Operator::Or => BinaryOp::Or,
            Operator::Modulus => BinaryOp::Modulus,
            _ => panic!("Operator {:?} cannot be converted to BinaryOp", op),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    LogicalNot,         // !
    ArithmeticNegative, // -
    ArithmeticPositive, // +
    BitwiseNot,         // ~
}
