// Lexer

use std::{
    iter::Peekable,
    str::{Chars, FromStr},
};

use crate::errors::LexerError;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Keyword(Keyword),

    // Identifiers
    Identifier(String),

    // Literals
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

    // Operators
    Operator(Operator),

    // Symbols/Delimiters
    Symbol(Symbol),

    // End of File
    EOF,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(keyword) => write!(f, "{:?}", keyword),
            Token::Identifier(name) => write!(f, "{}", name),
            Token::IntegerLiteral(value) => write!(f, "{}", value),
            Token::FloatLiteral(value) => write!(f, "{}", value),
            Token::StringLiteral(value) => write!(f, "{}", value),
            Token::Operator(operator) => write!(f, "{:?}", operator),
            Token::Symbol(symbol) => write!(f, "{:?}", symbol),
            Token::EOF => write!(f, "EOF"),
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
    While,  // while
    For,    // for
    Loop,   // loop
    Global, // global
    Block,  // block
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
            "while" => Ok(Keyword::While),
            "for" => Ok(Keyword::For),
            "loop" => Ok(Keyword::Loop),
            "global" => Ok(Keyword::Global),
            "block" => Ok(Keyword::Block),
            _ => Err(format!("{} is not a keyword.", s)),
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
    Concat,         // &
    And,            // &&
    Or,             // ||
    Pipe,           // >
    Modulus,        // %
    BitwiseNot,     // ~
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
    DataBracket,  // |
    Comma,        // ,
    Semicolon,    // ;
    Colon,        // :
    Dot,          // .
    Arrow,        // ->
}

#[inline]
fn is_skippable(src: char) -> bool {
    src.is_whitespace()
}

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, LexerError> {
    let estimated_capacity = source_code.len() / 5; // The number can be changed depending.
                                                    // A bigger number results in a smaller initial allocation size.
    let mut tokens = Vec::with_capacity(estimated_capacity);
    let mut chars = source_code.chars().peekable();
    while let Some(&c) = chars.peek() {
        if is_skippable(c) {
            chars.next();
            continue;
        }

        match c {
            '(' | ')' | '{' | '}' | '[' | ']' | ',' | ';' | ':' | '.' | '~' | '+' | '*' | '%'
            | '!' | '=' | '-' | '>' | '<' | '&' | '|' => {
                tokenize_operator_or_symbol(c, &mut chars, &mut tokens)?;
            }
            '"' => {
                tokenize_string_literal(&mut chars, &mut tokens)?;
            }
            '/' => {
                tokenize_comment_or_divide(&mut chars, &mut tokens)?;
            }
            _ if c.is_ascii_digit() => {
                tokenize_number(&mut chars, &mut tokens)?;
            }
            _ if c.is_ascii_alphabetic() || c == '_' => {
                tokenize_identifier_or_keyword(&mut chars, &mut tokens)?;
            }
            _ => return Err(LexerError::UnrecognizedCharacter(c)),
        }
    }

    tokens.push(Token::EOF);
    Ok(tokens)
}

fn tokenize_comment_or_divide(
    chars: &mut Peekable<Chars>,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
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
            tokens.push(Token::Operator(Operator::Divide));
        }
    }
    Ok(())
}

fn tokenize_operator_or_symbol(
    c: char,
    chars: &mut Peekable<Chars>,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
    match c {
        '(' => tokens.push(Token::Symbol(Symbol::LeftParen)),
        ')' => tokens.push(Token::Symbol(Symbol::RightParen)),
        '{' => tokens.push(Token::Symbol(Symbol::LeftBrace)),
        '}' => tokens.push(Token::Symbol(Symbol::RightBrace)),
        '[' => tokens.push(Token::Symbol(Symbol::LeftBracket)),
        ']' => tokens.push(Token::Symbol(Symbol::RightBracket)),
        ',' => tokens.push(Token::Symbol(Symbol::Comma)),
        ';' => tokens.push(Token::Symbol(Symbol::Semicolon)),
        ':' => tokens.push(Token::Symbol(Symbol::Colon)),
        '.' => tokens.push(Token::Symbol(Symbol::Dot)),
        '~' => tokens.push(Token::Operator(Operator::BitwiseNot)),
        '+' => tokens.push(Token::Operator(Operator::Add)),
        '*' => tokens.push(Token::Operator(Operator::Multiply)),
        '%' => tokens.push(Token::Operator(Operator::Modulus)),
        '!' | '=' | '-' | '>' | '<' | '&' | '|' => {
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
    chars: &mut Peekable<Chars>,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
    // Current character already known, peek next to decide on multi-character operators
    match c {
        '!' => {
            if matches!(chars.peek(), Some(&'=')) {
                chars.next(); // Consume '='
                tokens.push(Token::Operator(Operator::NotEqual));
                Ok(())
            } else {
                tokens.push(Token::Operator(Operator::LogicalNot));
                Ok(())
            }
        }
        '=' => {
            if matches!(chars.peek(), Some(&'=')) {
                chars.next(); // Consume '='
                tokens.push(Token::Operator(Operator::Equal));
                Ok(())
            } else {
                tokens.push(Token::Operator(Operator::Assign));
                Ok(())
            }
        }
        '-' => {
            if matches!(chars.peek(), Some(&'>')) {
                chars.next(); // Consume '>'
                tokens.push(Token::Symbol(Symbol::Arrow)); // '->'
                Ok(())
            } else {
                tokens.push(Token::Operator(Operator::Subtract));
                Ok(())
            }
        }
        '>' => {
            match chars.peek() {
                Some(&'=') => {
                    chars.next(); // Consume '='
                    tokens.push(Token::Operator(Operator::GreaterOrEqual));
                    Ok(())
                }
                Some(&'>') => {
                    chars.next(); // Consume '>'
                    tokens.push(Token::Operator(Operator::GreaterThan));
                    Ok(())
                }
                _ => {
                    tokens.push(Token::Operator(Operator::Pipe));
                    Ok(())
                }
            }
        }
        '<' => {
            match chars.peek() {
                Some(&'=') => {
                    chars.next(); // Consume '='
                    tokens.push(Token::Operator(Operator::LessOrEqual));
                    Ok(())
                }
                Some(&'<') => {
                    chars.next(); // Consume '<'
                    tokens.push(Token::Operator(Operator::LessThan));
                    Ok(())
                }
                _ => Err(LexerError::UnrecognizedCharacter(c)), // Handle error or unexpected character
            }
        }
        '&' => {
            if matches!(chars.peek(), Some(&'&')) {
                chars.next(); // Consume '&'
                tokens.push(Token::Operator(Operator::And));
                Ok(())
            } else {
                tokens.push(Token::Operator(Operator::Concat));
                Ok(())
            }
        }
        '|' => {
            if matches!(chars.peek(), Some(&'|')) {
                chars.next(); // Consume '|'
                tokens.push(Token::Operator(Operator::Or));
                Ok(())
            } else {
                tokens.push(Token::Symbol(Symbol::DataBracket));
                Ok(())
            }
        }
        _ => Err(LexerError::InternalError(format!(
            "handle_complex_operators called with unexpected character: {}",
            c
        ))),
    }
}

fn tokenize_number(chars: &mut Peekable<Chars>, tokens: &mut Vec<Token>) -> Result<(), LexerError> {
    let mut number = String::new();
    while let Some(&next) = chars.peek() {
        if next.is_ascii_digit() || next == '.' {
            number.push(chars.next().unwrap());
        } else {
            break;
        }
    }
    if number.contains('.') {
        tokens.push(Token::FloatLiteral(
            number
                .parse()
                .map_err(|_| LexerError::InvalidNumberFormat(number.clone()))?,
        ));
    } else {
        tokens.push(Token::IntegerLiteral(
            number
                .parse()
                .map_err(|_| LexerError::InvalidNumberFormat(number.clone()))?,
        ));
    }
    Ok(())
}

fn tokenize_identifier_or_keyword(
    chars: &mut Peekable<Chars>,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
    let mut identifier = String::new();
    while let Some(&next) = chars.peek() {
        if next.is_ascii_alphanumeric() || next == '_' {
            identifier.push(chars.next().unwrap());
        } else {
            break;
        }
    }
    match Keyword::from_str(&identifier) {
        Ok(keyword) => {
            tokens.push(Token::Keyword(keyword));
            Ok(())
        }
        Err(_) => {
            tokens.push(Token::Identifier(identifier));
            Ok(())
        }
    }
}

fn tokenize_string_literal(
    chars: &mut Peekable<Chars>,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
    chars.next(); // Consume the initial quote
    let mut literal = String::new();
    while let Some(&ch) = chars.peek() {
        match ch {
            '"' => {
                chars.next(); // Consume the closing quote
                tokens.push(Token::StringLiteral(literal));
                return Ok(());
            }
            '\\' => {
                chars.next(); // Consume the backslash
                if let Some(&escaped) = chars.peek() {
                    literal.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        _ => escaped,
                    });
                    chars.next(); // Consume the escaped character
                }
            }
            _ => {
                literal.push(ch);
                chars.next();
            }
        }
    }
    Err(LexerError::UnterminatedStringLiteral)
}
