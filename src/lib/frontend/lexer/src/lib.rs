//! # Lexer
//!
//! The lexer is responsible for converting the source code into a stream of tokens.
//!
//! [`Return to Frontend Module`](../index.html)
//!
//! ## Logic
//!
//! The lexer works by iterating over the source code character by character.
//!
//! The lexer recognizes different types of tokens:
//! - Keywords
//! - Identifiers
//! - Literals
//! - Operators
//! - Symbols/Delimiters
//! - End of File

pub mod errors;

use std::{
    iter::Peekable,
    str::{Chars, FromStr},
};

use quiklang_utils::Span;

use crate::errors::LexerError;

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
    StringInterpolationStart,
    StringInterpolationEnd,

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
            TokenType::StringInterpolationStart => write!(f, "String Interpolation Start"),
            TokenType::StringInterpolationEnd => write!(f, "String Interpolation End"),
            TokenType::Operator(operator) => write!(f, "{}", operator),
            TokenType::Symbol(symbol) => write!(f, "{}", symbol),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}

// Keywords
#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Import, // import
    Fn,     // fn
    Let,    // let
    Mut,    // mut
    Const,  // const
    Return, // return
    If,     // if
    Else,   // else
    Err,    // Err
    Ok,     // Ok
    Exit,   // exit
    Break,  // break
    Async,  // async
    Await,  // await
    While,  // while
    For,    // for
    Loop,   // loop
    Global, // global
    Block,  // block
    In,     // in
    Struct, // struct
    Enum,   // enum
    Type,   // type
    Impl,   // impl
    Extern, // extern
    As,     // as
    Clone,  // clone
    Ref,    // ref
    MutRef, // mutref
    Extend, // extend
}

impl FromStr for Keyword {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, <Keyword as FromStr>::Err> {
        match s {
            "import" => Ok(Keyword::Import),
            "fn" => Ok(Keyword::Fn),
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
            _ => Err(format!("{} is not a keyword.", s)),
        }
    }
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Import => write!(f, "Keyword 'import'"),
            Keyword::Fn => write!(f, "Keyword 'fn'"),
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
        }
    }
}

// Operators
#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Assign,         // =
    Add,            // +
    Subtract,       // -
    Multiply,       // *
    Divide,         // /
    GreaterThan,    // >>
    LessThan,       // <<
    GreaterOrEqual, // >=
    LessOrEqual,    // <=
    Equal,          // ==
    NotEqual,       // !=
    LogicalNot,     // !
    BitwiseAnd,     // &
    And,            // &&
    Or,             // ||
    Pipe,           // >
    Modulus,        // %
    BitwiseNot,     // ~
    RangeExclusive, // ..
    RangeInclusive, // ..=
    BitwiseOr,      // |
    BitwiseXor,     // ^
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
        }
    }
}

#[inline]
fn is_skippable(src: char) -> bool {
    src.is_whitespace()
}

// Custom made struct to implement line and col.
struct CharStream<'a> {
    chars: Peekable<Chars<'a>>,
    current_pos: usize, // Byte offset from the beginning
    line: usize,
    col: usize,
}

impl<'a> CharStream<'a> {
    fn new(source_code: &'a str) -> Self {
        CharStream {
            chars: source_code.chars().peekable(),
            current_pos: 0,
            line: 1,
            col: 1,
        }
    }

    fn next(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            let c_len = c.len_utf8();
            self.current_pos += c_len;
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            Some(c)
        } else {
            None
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn get_position(&self) -> (usize, usize, usize) {
        (self.current_pos, self.line, self.col)
    }
}

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, LexerError> {
    let mut chars: CharStream = CharStream::new(source_code);
    let mut tokens: Vec<Token> = Vec::with_capacity(source_code.len() + 100);

    while let Some(&c) = chars.peek() {
        tokenize_chars(c, &mut chars, &mut tokens)?;
    }

    tokens.push(Token {
        token: TokenType::EOF,
        // line: chars.line,
        // col: chars.col,
        span: Span::new(
            chars.current_pos as u32,
            chars.current_pos as u32,
            chars.line as u32,
            chars.col as u32,
        ),
    });

    println!("{:#?}", tokens);

    Ok(tokens)
}

fn tokenize_chars(
    c: char,
    chars: &mut CharStream,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
    if is_skippable(c) {
        chars.next();
        return Ok(());
    }

    match c {
        '(' | ')' | '{' | '}' | '[' | ']' | ',' | ';' | ':' | '.' | '~' | '+' | '*' | '%' | '!'
        | '=' | '-' | '>' | '<' | '&' | '|' | '@' | '^' => {
            tokenize_operator_or_symbol(c, chars, tokens)?;
        }
        '"' => {
            tokenize_string_literal(chars, tokens)?;
        }
        '/' => {
            tokenize_comment_or_divide(chars, tokens)?;
        }
        _ if c.is_ascii_digit() => {
            tokenize_number(chars, tokens)?;
        }
        _ if c.is_ascii_alphabetic() || c == '_' => {
            tokenize_identifier_or_keyword(chars, tokens)?;
        }
        _ => {
            return Err(LexerError::UnrecognizedCharacter {
                character: c,
                line: chars.line,
                col: chars.col,
            })
        }
    }

    Ok(())
}

fn tokenize_comment_or_divide(
    chars: &mut CharStream,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
    let before_pos = chars.get_position().0;
    chars.next(); // Consume the '/'
    match chars.peek() {
        Some('/') => {
            // Single-line comment
            chars.next(); // Consume the second '/'
            while let Some(&ch) = chars.peek() {
                if ch == '\n' {
                    break; // End of comment
                }
                chars.next();
            }
        }
        Some('*') => {
            // Multi-line comment
            chars.next(); // Consume the '*'
            loop {
                match (chars.next(), chars.peek()) {
                    (Some('*'), Some('/')) => {
                        chars.next(); // Consume the '/'
                        break; // End of comment
                    }
                    (Some(_), None) => break, // EOF without closing comment
                    (None, _) => break,       // Handle EOF gracefully
                    _ => {}                   // Continue scanning
                }
            }
        }
        _ => {
            // It's a divide operator
            tokens.push(Token {
                token: TokenType::Operator(Operator::Divide),
                // line: chars.line,
                // col: chars.col,
                span: Span::new(
                    before_pos as u32,
                    chars.current_pos as u32,
                    chars.line as u32,
                    chars.col as u32,
                ),
            });
        }
    }
    Ok(())
}

fn tokenize_operator_or_symbol(
    c: char,
    chars: &mut CharStream,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
    match c {
        '(' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::LeftParen),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        ')' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::RightParen),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        '{' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::LeftBrace),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        '}' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::RightBrace),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        '[' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::LeftBracket),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        ']' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::RightBracket),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        ',' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::Comma),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        ';' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::Semicolon),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        ':' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::Colon),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        // '.' => tokens.push(Token {
        //     token: TokenType::Symbol(Symbol::Dot),
        //     line: chars.line,
        //     col: chars.col,
        // }),
        '~' => tokens.push(Token {
            token: TokenType::Operator(Operator::BitwiseNot),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        '+' => tokens.push(Token {
            token: TokenType::Operator(Operator::Add),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        '*' => tokens.push(Token {
            token: TokenType::Operator(Operator::Multiply),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        '%' => tokens.push(Token {
            token: TokenType::Operator(Operator::Modulus),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        '@' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::At),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        '^' => tokens.push(Token {
            token: TokenType::Operator(Operator::BitwiseXor),
            span: Span::new(
                chars.current_pos as u32 - 1,
                chars.current_pos as u32,
                chars.line as u32,
                chars.col as u32,
            ),
        }),
        '!' | '=' | '-' | '>' | '<' | '&' | '|' | '.' => {
            chars.next();
            handle_complex_operators(c, chars, tokens)?;
            return Ok(());
        }
        _ => {
            return Err(LexerError::InternalError(format!(
                "Unresolved character {}",
                c
            )))
        }
    }
    chars.next();
    Ok(())
}

fn handle_complex_operators(
    c: char,
    chars: &mut CharStream,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
    // Current character already known, peek next to decide on multi-character operators
    let before_pos = chars.get_position().0 - 1;
    match c {
        '!' => {
            if matches!(chars.peek(), Some(&'=')) {
                chars.next(); // Consume '='
                tokens.push(Token {
                    token: TokenType::Operator(Operator::NotEqual),
                    span: Span::new(
                        before_pos as u32,
                        chars.current_pos as u32,
                        chars.line as u32,
                        chars.col as u32,
                    ),
                });
                Ok(())
            } else {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::LogicalNot),
                    span: Span::new(
                        before_pos as u32,
                        chars.current_pos as u32,
                        chars.line as u32,
                        chars.col as u32,
                    ),
                });
                Ok(())
            }
        }
        '=' => {
            if matches!(chars.peek(), Some(&'=')) {
                chars.next(); // Consume '='
                tokens.push(Token {
                    token: TokenType::Operator(Operator::Equal),
                    span: Span::new(
                        before_pos as u32,
                        chars.current_pos as u32,
                        chars.line as u32,
                        chars.col as u32,
                    ),
                });

                Ok(())
            } else {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::Assign),
                    span: Span::new(
                        before_pos as u32,
                        chars.current_pos as u32,
                        chars.line as u32,
                        chars.col as u32,
                    ),
                });
                Ok(())
            }
        }
        '-' => {
            if matches!(chars.peek(), Some(&'>')) {
                chars.next(); // Consume '>'
                tokens.push(Token {
                    token: TokenType::Symbol(Symbol::Arrow),
                    span: Span::new(
                        before_pos as u32,
                        chars.current_pos as u32,
                        chars.line as u32,
                        chars.col as u32,
                    ),
                });

                Ok(())
            } else {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::Subtract),
                    span: Span::new(
                        before_pos as u32,
                        chars.current_pos as u32,
                        chars.line as u32,
                        chars.col as u32,
                    ),
                });
                Ok(())
            }
        }
        '>' => {
            match chars.peek() {
                Some(&'=') => {
                    chars.next(); // Consume '='
                    tokens.push(Token {
                        token: TokenType::Operator(Operator::GreaterOrEqual),
                        span: Span::new(
                            before_pos as u32,
                            chars.current_pos as u32,
                            chars.line as u32,
                            chars.col as u32,
                        ),
                    });

                    Ok(())
                }
                _ => {
                    tokens.push(Token {
                        token: TokenType::Operator(Operator::GreaterThan),
                        span: Span::new(
                            before_pos as u32,
                            chars.current_pos as u32,
                            chars.line as u32,
                            chars.col as u32,
                        ),
                    });
                    Ok(())
                }
            }
        }
        '<' => {
            match chars.peek() {
                Some(&'=') => {
                    chars.next(); // Consume '='
                    tokens.push(Token {
                        token: TokenType::Operator(Operator::LessOrEqual),
                        span: Span::new(
                            before_pos as u32,
                            chars.current_pos as u32,
                            chars.line as u32,
                            chars.col as u32,
                        ),
                    });

                    Ok(())
                }
                _ => {
                    tokens.push(Token {
                        token: TokenType::Operator(Operator::LessThan),
                        span: Span::new(
                            before_pos as u32,
                            chars.current_pos as u32,
                            chars.line as u32,
                            chars.col as u32,
                        ),
                    });
                    Ok(())
                }
            }
        }
        '&' => {
            if matches!(chars.peek(), Some(&'&')) {
                chars.next(); // Consume '&'
                tokens.push(Token {
                    token: TokenType::Operator(Operator::And),
                    span: Span::new(
                        before_pos as u32,
                        chars.current_pos as u32,
                        chars.line as u32,
                        chars.col as u32,
                    ),
                });

                Ok(())
            } else {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::BitwiseAnd),
                    span: Span::new(
                        before_pos as u32,
                        chars.current_pos as u32,
                        chars.line as u32,
                        chars.col as u32,
                    ),
                });
                Ok(())
            }
        }
        '|' => {
            if matches!(chars.peek(), Some(&'|')) {
                chars.next(); // Consume '|'
                tokens.push(Token {
                    token: TokenType::Operator(Operator::Or),
                    span: Span::new(
                        before_pos as u32,
                        chars.current_pos as u32,
                        chars.line as u32,
                        chars.col as u32,
                    ),
                });

                Ok(())
            } else {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::BitwiseOr),
                    span: Span::new(
                        before_pos as u32,
                        chars.current_pos as u32,
                        chars.line as u32,
                        chars.col as u32,
                    ),
                });
                Ok(())
            }
        }
        '.' => {
            if matches!(chars.peek(), Some(&'.')) {
                chars.next();
                if matches!(chars.peek(), Some(&'=')) {
                    chars.next(); // Consume '='
                    tokens.push(Token {
                        token: TokenType::Operator(Operator::RangeInclusive),
                        span: Span::new(
                            before_pos as u32,
                            chars.current_pos as u32,
                            chars.line as u32,
                            chars.col as u32,
                        ),
                    });
                } else {
                    tokens.push(Token {
                        token: TokenType::Operator(Operator::RangeExclusive),
                        span: Span::new(
                            before_pos as u32,
                            chars.current_pos as u32,
                            chars.line as u32,
                            chars.col as u32,
                        ),
                    });
                }
            } else {
                tokens.push(Token {
                    token: TokenType::Symbol(Symbol::Dot),
                    span: Span::new(
                        before_pos as u32,
                        chars.current_pos as u32,
                        chars.line as u32,
                        chars.col as u32,
                    ),
                });
            }
            Ok(())
        }
        _ => Err(LexerError::InternalError(format!(
            "handle_complex_operators called with unexpected character: {} @ {} {}",
            c, chars.line, chars.col
        ))),
    }
}

fn tokenize_number(chars: &mut CharStream, tokens: &mut Vec<Token>) -> Result<(), LexerError> {
    let mut number = String::new();
    let starting_line = chars.line;
    let starting_col = chars.col;
    let starting_pos = chars.get_position().0;
    while let Some(&next) = chars.peek() {
        if next.is_ascii_digit() || next == '.' {
            number.push(chars.next().unwrap());
        } else {
            break;
        }
    }
    if number.contains('.') {
        tokens.push(Token {
            token: TokenType::FloatLiteral(number.parse().map_err(|_| {
                LexerError::InvalidNumberFormat {
                    invalid_string: number.clone(),
                    line: starting_line,
                    col: starting_col,
                }
            })?),
            span: Span::new(
                starting_pos as u32,
                chars.current_pos as u32,
                starting_line as u32,
                starting_col as u32,
            ),
        });
    } else {
        tokens.push(Token {
            token: TokenType::IntegerLiteral(number.parse().map_err(|_| {
                LexerError::InvalidNumberFormat {
                    invalid_string: number.clone(),
                    line: starting_line,
                    col: starting_col,
                }
            })?),
            span: Span::new(
                starting_pos as u32,
                chars.current_pos as u32,
                starting_line as u32,
                starting_col as u32,
            ),
        });
    }
    Ok(())
}

fn tokenize_identifier_or_keyword(
    chars: &mut CharStream,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
    let mut identifier = String::new();
    let starting_line = chars.line;
    let starting_col = chars.col;
    let starting_pos = chars.get_position().0;
    while let Some(&next) = chars.peek() {
        if next.is_ascii_alphanumeric() || next == '_' {
            identifier.push(chars.next().unwrap());
        } else {
            break;
        }
    }
    match Keyword::from_str(&identifier) {
        Ok(keyword) => {
            tokens.push(Token {
                token: TokenType::Keyword(keyword),
                span: Span::new(
                    starting_pos as u32,
                    chars.current_pos as u32,
                    starting_line as u32,
                    starting_col as u32,
                ),
            });
            Ok(())
        }
        Err(_) => {
            tokens.push(Token {
                token: TokenType::Identifier(identifier),
                span: Span::new(
                    starting_pos as u32,
                    chars.current_pos as u32,
                    starting_line as u32,
                    starting_col as u32,
                ),
            });
            Ok(())
        }
    }
}

fn tokenize_string_literal(
    chars: &mut CharStream,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
    let mut start_span = Span::new(
        chars.current_pos as u32,
        0,
        chars.line as u32,
        chars.col as u32,
    ); // Temporary end
    chars.next(); // Consume the initial quote
    let mut literal = String::new();
    let mut current_pos = chars.current_pos;
    let mut current_line = chars.line;
    let mut current_col = chars.col;

    while let Some(&ch) = chars.peek() {
        match ch {
            '"' => {
                chars.next(); // Consume the closing quote
                let end_span = Span::new(
                    current_pos as u32,
                    chars.current_pos as u32,
                    current_line as u32,
                    current_col as u32,
                );
                tokens.push(Token {
                    token: TokenType::StringLiteral(literal),
                    span: Span::new(
                        start_span.start,
                        end_span.end,
                        start_span.line,
                        start_span.col,
                    ),
                });
                return Ok(());
            }
            '{' => {
                chars.next(); // Consume '{'
                let end_span = Span::new(
                    current_pos as u32,
                    chars.current_pos as u32,
                    current_line as u32,
                    current_col as u32,
                );
                tokens.push(Token {
                    token: TokenType::StringLiteral(literal.clone()),
                    span: Span::new(
                        start_span.start,
                        end_span.end,
                        start_span.line,
                        start_span.col,
                    ),
                });
                tokens.push(Token {
                    token: TokenType::StringInterpolationStart,
                    span: end_span,
                });
                literal.clear();
                start_span.start = chars.current_pos as u32;
            }
            _ => {
                literal.push(ch);
                chars.next();
                current_pos = chars.current_pos;
                current_line = chars.line;
                current_col = chars.col;
            }
        }
    }

    Err(LexerError::UnterminatedStringLiteral {
        line: start_span.line as usize,
        col: start_span.col as usize,
    })
}
