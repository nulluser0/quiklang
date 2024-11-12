use std::str::FromStr;

use crate::errors::Span;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token: TokenType,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Keywords
    Keyword(Keyword),

    // Identifiers
    Identifier(String),

    // Literals
    IntegerLiteral(i64),
    FloatLiteral(f64),

    // String literals and interpolation
    StringLiteral(String),

    // String interpolation
    ComplexStringStart, // Indicates the start of a complex string (string interpolation) '"...'
    StringInterpolationStart, // Indicates the start of a string interpolation '{'
    StringInterpolationEnd, // Indicates the end of a string interpolation '}'
    ComplexStringEnd,   // Indicates the end of a complex string (string interpolation) '..."'

    // Operators
    Operator(Operator),

    // Symbols/Delimiters
    Symbol(Symbol),

    // End of File
    EOF,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Keyword(keyword) => write!(f, "{}", keyword),
            TokenType::Identifier(name) => write!(f, "{}", name),
            TokenType::IntegerLiteral(value) => write!(f, "{}", value),
            TokenType::FloatLiteral(value) => write!(f, "{}", value),
            TokenType::StringLiteral(value) => write!(f, "{}", value),
            TokenType::ComplexStringStart => write!(f, "Complex String Start"),
            TokenType::StringInterpolationStart => write!(f, "String Interpolation Start"),
            TokenType::StringInterpolationEnd => write!(f, "String Interpolation End"),
            TokenType::ComplexStringEnd => write!(f, "Complex String End"),
            TokenType::Operator(operator) => write!(f, "{}", operator),
            TokenType::Symbol(symbol) => write!(f, "{}", symbol),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}

// Keywords
#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Fun,     // fun
    Let,     // let
    Mut,     // mut
    Const,   // const
    Return,  // return
    If,      // if
    Else,    // else
    Err,     // Err
    Ok,      // Ok
    Exit,    // exit
    Break,   // break
    Async,   // async
    Await,   // await
    While,   // while
    For,     // for
    Loop,    // loop
    Global,  // global
    Block,   // block
    In,      // in
    Struct,  // struct
    Enum,    // enum
    Type,    // type
    Impl,    // impl
    Extern,  // extern
    As,      // as
    Clone,   // clone
    Ref,     // ref
    MutRef,  // mutref
    Extend,  // extend
    Pub,     // pub
    Super,   // super
    Package, // package
    Trait,   // trait
    Module,  // module
    List,    // list
    Bool,    // bool
    True,    // true
    False,   // false
    Null,    // null
    Void,    // void
    Integer, // integer
    Float,   // float
    String,  // string
    Char,    // char
    Use,     // use
}

impl FromStr for Keyword {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, <Keyword as FromStr>::Err> {
        match s {
            "fun" => Ok(Keyword::Fun),
            "let" => Ok(Keyword::Let),
            "mut" => Ok(Keyword::Mut),
            "const" => Ok(Keyword::Const),
            "return" => Ok(Keyword::Return),
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "Err" => Ok(Keyword::Err),
            "Ok" => Ok(Keyword::Ok),
            "exit" => Ok(Keyword::Exit),
            "break" => Ok(Keyword::Break),
            "async" => Ok(Keyword::Async),
            "await" => Ok(Keyword::Await),
            "while" => Ok(Keyword::While),
            "for" => Ok(Keyword::For),
            "loop" => Ok(Keyword::Loop),
            "global" => Ok(Keyword::Global),
            "block" => Ok(Keyword::Block),
            "in" => Ok(Keyword::In),
            "struct" => Ok(Keyword::Struct),
            "enum" => Ok(Keyword::Enum),
            "type" => Ok(Keyword::Type),
            "impl" => Ok(Keyword::Impl),
            "extern" => Ok(Keyword::Extern),
            "as" => Ok(Keyword::As),
            "clone" => Ok(Keyword::Clone),
            "ref" => Ok(Keyword::Ref),
            "mutref" => Ok(Keyword::MutRef),
            "extend" => Ok(Keyword::Extend),
            "pub" => Ok(Keyword::Pub),
            "super" => Ok(Keyword::Super),
            "package" => Ok(Keyword::Package),
            "trait" => Ok(Keyword::Trait),
            "module" => Ok(Keyword::Module),
            "list" => Ok(Keyword::List),
            "bool" => Ok(Keyword::Bool),
            "true" => Ok(Keyword::True),
            "false" => Ok(Keyword::False),
            "null" => Ok(Keyword::Null),
            "void" => Ok(Keyword::Void),
            "integer" => Ok(Keyword::Integer),
            "float" => Ok(Keyword::Float),
            "string" => Ok(Keyword::String),
            "char" => Ok(Keyword::Char),
            "use" => Ok(Keyword::Use),
            _ => Err(format!("{} is not a keyword.", s)),
        }
    }
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Fun => write!(f, "Keyword 'fun'"),
            Keyword::Let => write!(f, "Keyword 'let'"),
            Keyword::Mut => write!(f, "Keyword 'mut'"),
            Keyword::Const => write!(f, "Keyword 'const'"),
            Keyword::Return => write!(f, "Keyword 'return'"),
            Keyword::If => write!(f, "Keyword 'if'"),
            Keyword::Else => write!(f, "Keyword 'else'"),
            Keyword::Err => write!(f, "Keyword 'Err'"),
            Keyword::Ok => write!(f, "Keyword 'Ok'"),
            Keyword::Exit => write!(f, "Keyword 'exit'"),
            Keyword::Break => write!(f, "Keyword 'break'"),
            Keyword::Async => write!(f, "Keyword 'async'"),
            Keyword::Await => write!(f, "Keyword 'await'"),
            Keyword::While => write!(f, "Keyword 'while'"),
            Keyword::For => write!(f, "Keyword 'for'"),
            Keyword::Loop => write!(f, "Keyword 'loop'"),
            Keyword::Global => write!(f, "Keyword 'global'"),
            Keyword::Block => write!(f, "Keyword 'block'"),
            Keyword::In => write!(f, "Keyword 'in'"),
            Keyword::Struct => write!(f, "Keyword 'struct'"),
            Keyword::Enum => write!(f, "Keyword 'enum'"),
            Keyword::Type => write!(f, "Keyword 'type'"),
            Keyword::Impl => write!(f, "Keyword 'impl'"),
            Keyword::Extern => write!(f, "Keyword 'extern'"),
            Keyword::As => write!(f, "Keyword 'as'"),
            Keyword::Clone => write!(f, "Keyword 'clone'"),
            Keyword::Ref => write!(f, "Keyword 'ref'"),
            Keyword::MutRef => write!(f, "Keyword 'mutref'"),
            Keyword::Extend => write!(f, "Keyword 'extend'"),
            Keyword::Pub => write!(f, "Keyword 'pub'"),
            Keyword::Super => write!(f, "Keyword 'super'"),
            Keyword::Package => write!(f, "Keyword 'package'"),
            Keyword::Trait => write!(f, "Keyword 'trait'"),
            Keyword::Module => write!(f, "Keyword 'module'"),
            Keyword::List => write!(f, "Keyword 'list'"),
            Keyword::Bool => write!(f, "Keyword 'bool'"),
            Keyword::True => write!(f, "Keyword 'true'"),
            Keyword::False => write!(f, "Keyword 'false'"),
            Keyword::Null => write!(f, "Keyword 'null'"),
            Keyword::Void => write!(f, "Keyword 'void'"),
            Keyword::Integer => write!(f, "Keyword 'integer'"),
            Keyword::Float => write!(f, "Keyword 'float'"),
            Keyword::String => write!(f, "Keyword 'string'"),
            Keyword::Char => write!(f, "Keyword 'char'"),
            Keyword::Use => write!(f, "Keyword 'use'"),
        }
    }
}

// Operators
#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Assign,            // =
    Add,               // +
    Subtract,          // -
    Multiply,          // *
    Divide,            // /
    GreaterThan,       // >
    LessThan,          // <
    GreaterOrEqual,    // >=
    LessOrEqual,       // <=
    Equal,             // ==
    NotEqual,          // !=
    LogicalNot,        // !
    BitwiseAnd,        // &
    And,               // &&
    Or,                // ||
    Pipe,              // >
    Modulus,           // %
    BitwiseNot,        // ~
    RangeExclusive,    // ..
    RangeInclusive,    // ..=
    BitwiseOr,         // |
    BitwiseXor,        // ^
    ShiftLeft,         // <<
    ShiftRight,        // >>
    QuestionMark,      // ?
    DoubleQuestion,    // ??
    DoubleExclamation, // !!
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Assign => write!(f, "Operator Assign '='"),
            Operator::Add => write!(f, "Operator Add '+'"),
            Operator::Subtract => write!(f, "Operator Subtract '-'"),
            Operator::Multiply => write!(f, "Operator Multiply '*'"),
            Operator::Divide => write!(f, "Operator Divide '/'"),
            Operator::GreaterThan => write!(f, "Operator Greater Than '>'"),
            Operator::LessThan => write!(f, "Operator Less Than '<'"),
            Operator::GreaterOrEqual => write!(f, "Operator Greater or Equal '>='"),
            Operator::LessOrEqual => write!(f, "Operator Less or Equal '<='"),
            Operator::Equal => write!(f, "Operator Equal '=='"),
            Operator::NotEqual => write!(f, "Operator Not Equal '!='"),
            Operator::LogicalNot => write!(f, "Operator Logical Not '!'"),
            Operator::BitwiseAnd => write!(f, "Operator Bitwise And '&'"),
            Operator::And => write!(f, "Operator And '&&'"),
            Operator::Or => write!(f, "Operator Or '||'"),
            Operator::Pipe => write!(f, "Operator Pipe '|'"),
            Operator::Modulus => write!(f, "Operator Modulus '%'"),
            Operator::BitwiseNot => write!(f, "Operator Bitwise Not '~'"),
            Operator::RangeExclusive => write!(f, "Operator Range Exclusive '..'"),
            Operator::RangeInclusive => write!(f, "Operator Range Inclusive '..='"),
            Operator::BitwiseOr => write!(f, "Operator Bitwise Or '|'"),
            Operator::BitwiseXor => write!(f, "Operator Bitwise Xor '^'"),
            Operator::ShiftLeft => write!(f, "Operator Shift Left '<<'"),
            Operator::ShiftRight => write!(f, "Operator Shift Right '>>'"),
            Operator::QuestionMark => write!(f, "Operator Question Mark '?'"),
            Operator::DoubleQuestion => write!(f, "Operator Double Question '??'"),
            Operator::DoubleExclamation => write!(f, "Operator Double Exclamation '!!'"),
        }
    }
}

// Symbols/Delimiters
#[derive(Debug, PartialEq, Clone)]
pub enum Symbol {
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Semicolon,    // ;
    Colon,        // :
    Dot,          // .
    Arrow,        // ->
    At,           // @
    DoubleColon,  // ::
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::LeftParen => write!(f, "Symbol Left Parenthesis '('"),
            Symbol::RightParen => write!(f, "Symbol Right Parenthesis ')'"),
            Symbol::LeftBrace => write!(f, "Symbol Left Brace '{{'"),
            Symbol::RightBrace => write!(f, "Symbol Right Brace '}}'"),
            Symbol::LeftBracket => write!(f, "Symbol Left Bracket '['"),
            Symbol::RightBracket => write!(f, "Symbol Right Bracket ']'"),
            Symbol::Comma => write!(f, "Symbol Comma ','"),
            Symbol::Semicolon => write!(f, "Symbol Semicolon ';'"),
            Symbol::Colon => write!(f, "Symbol Colon ':'"),
            Symbol::Dot => write!(f, "Symbol Dot '.'"),
            Symbol::Arrow => write!(f, "Symbol Arrow '->'"),
            Symbol::At => write!(f, "Symbol At '@'"),
            Symbol::DoubleColon => write!(f, "Symbol Double Colon '::'"),
        }
    }
}