// Lexer

use std::{
    iter::Peekable,
    str::{Chars, FromStr},
};

use crate::errors::LexerError;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token: TokenType,
    pub line: usize,
    pub col: usize,
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
    StringLiteral(String),

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
            TokenType::Keyword(keyword) => write!(f, "{:?}", keyword),
            TokenType::Identifier(name) => write!(f, "{}", name),
            TokenType::IntegerLiteral(value) => write!(f, "{}", value),
            TokenType::FloatLiteral(value) => write!(f, "{}", value),
            TokenType::StringLiteral(value) => write!(f, "{}", value),
            TokenType::Operator(operator) => write!(f, "{:?}", operator),
            TokenType::Symbol(symbol) => write!(f, "{:?}", symbol),
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

// Custom made struct to implement line and col.
struct CharStream<'a> {
    chars: Peekable<Chars<'a>>,
    line: usize,
    col: usize,
}

impl<'a> CharStream<'a> {
    fn new(source_code: &'a str) -> Self {
        CharStream {
            chars: source_code.chars().peekable(),
            line: 1,
            col: 1,
        }
    }

    fn next(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
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
}

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, LexerError> {
    let estimated_capacity = source_code.len() / 5; // The number can be changed depending.
                                                    // A bigger number results in a smaller initial allocation size.
    let mut tokens: Vec<Token> = Vec::with_capacity(estimated_capacity);
    let mut chars: CharStream = CharStream::new(source_code);
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
            _ => {
                return Err(LexerError::UnrecognizedCharacter {
                    character: c,
                    line: chars.line,
                    col: chars.col,
                })
            }
        }
    }

    tokens.push(Token {
        token: TokenType::EOF,
        line: chars.line,
        col: chars.col,
    });
    Ok(tokens)
}

fn tokenize_comment_or_divide(
    chars: &mut CharStream,
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
            tokens.push(Token {
                token: TokenType::Operator(Operator::Divide),
                line: chars.line,
                col: chars.col,
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
            line: chars.line,
            col: chars.col,
        }),
        ')' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::RightParen),
            line: chars.line,
            col: chars.col,
        }),
        '{' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::LeftBrace),
            line: chars.line,
            col: chars.col,
        }),
        '}' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::RightBrace),
            line: chars.line,
            col: chars.col,
        }),
        '[' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::LeftBracket),
            line: chars.line,
            col: chars.col,
        }),
        ']' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::RightBracket),
            line: chars.line,
            col: chars.col,
        }),
        ',' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::Comma),
            line: chars.line,
            col: chars.col,
        }),
        ';' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::Semicolon),
            line: chars.line,
            col: chars.col,
        }),
        ':' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::Colon),
            line: chars.line,
            col: chars.col,
        }),
        '.' => tokens.push(Token {
            token: TokenType::Symbol(Symbol::Dot),
            line: chars.line,
            col: chars.col,
        }),
        '~' => tokens.push(Token {
            token: TokenType::Operator(Operator::BitwiseNot),
            line: chars.line,
            col: chars.col,
        }),
        '+' => tokens.push(Token {
            token: TokenType::Operator(Operator::Add),
            line: chars.line,
            col: chars.col,
        }),
        '*' => tokens.push(Token {
            token: TokenType::Operator(Operator::Multiply),
            line: chars.line,
            col: chars.col,
        }),
        '%' => tokens.push(Token {
            token: TokenType::Operator(Operator::Modulus),
            line: chars.line,
            col: chars.col,
        }),
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
    chars: &mut CharStream,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
    // Current character already known, peek next to decide on multi-character operators
    match c {
        '!' => {
            if matches!(chars.peek(), Some(&'=')) {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::NotEqual),
                    line: chars.line,
                    col: chars.col,
                });
                chars.next(); // Consume '='
                Ok(())
            } else {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::LogicalNot),
                    line: chars.line,
                    col: chars.col,
                });
                Ok(())
            }
        }
        '=' => {
            if matches!(chars.peek(), Some(&'=')) {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::Equal),
                    line: chars.line,
                    col: chars.col,
                });
                chars.next(); // Consume '='
                Ok(())
            } else {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::Assign),
                    line: chars.line,
                    col: chars.col,
                });
                Ok(())
            }
        }
        '-' => {
            if matches!(chars.peek(), Some(&'>')) {
                tokens.push(Token {
                    token: TokenType::Symbol(Symbol::Arrow),
                    line: chars.line,
                    col: chars.col,
                });
                chars.next(); // Consume '>'
                Ok(())
            } else {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::Subtract),
                    line: chars.line,
                    col: chars.col,
                });
                Ok(())
            }
        }
        '>' => {
            match chars.peek() {
                Some(&'=') => {
                    tokens.push(Token {
                        token: TokenType::Operator(Operator::GreaterOrEqual),
                        line: chars.line,
                        col: chars.col,
                    });
                    chars.next(); // Consume '='
                    Ok(())
                }
                _ => {
                    tokens.push(Token {
                        token: TokenType::Operator(Operator::GreaterThan),
                        line: chars.line,
                        col: chars.col,
                    });
                    Ok(())
                }
            }
        }
        '<' => {
            match chars.peek() {
                Some(&'=') => {
                    tokens.push(Token {
                        token: TokenType::Operator(Operator::LessOrEqual),
                        line: chars.line,
                        col: chars.col,
                    });
                    chars.next(); // Consume '='
                    Ok(())
                }
                _ => {
                    tokens.push(Token {
                        token: TokenType::Operator(Operator::LessThan),
                        line: chars.line,
                        col: chars.col,
                    });
                    Ok(())
                }
            }
        }
        '&' => {
            if matches!(chars.peek(), Some(&'&')) {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::And),
                    line: chars.line,
                    col: chars.col,
                });
                chars.next(); // Consume '&'
                Ok(())
            } else {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::Concat),
                    line: chars.line,
                    col: chars.col,
                });
                Ok(())
            }
        }
        '|' => {
            if matches!(chars.peek(), Some(&'|')) {
                tokens.push(Token {
                    token: TokenType::Operator(Operator::Or),
                    line: chars.line,
                    col: chars.col,
                });
                chars.next(); // Consume '|'
                Ok(())
            } else {
                tokens.push(Token {
                    token: TokenType::Symbol(Symbol::DataBracket),
                    line: chars.line,
                    col: chars.col,
                });
                Ok(())
            }
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
            line: starting_line,
            col: starting_col,
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
            line: starting_line,
            col: starting_col,
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
                line: starting_line,
                col: starting_col,
            });
            Ok(())
        }
        Err(_) => {
            tokens.push(Token {
                token: TokenType::Identifier(identifier),
                line: starting_line,
                col: starting_col,
            });
            Ok(())
        }
    }
}

fn tokenize_string_literal(
    chars: &mut CharStream,
    tokens: &mut Vec<Token>,
) -> Result<(), LexerError> {
    let starting_line = chars.line;
    let starting_col = chars.col;
    chars.next(); // Consume the initial quote
    let mut literal = String::new();
    while let Some(&ch) = chars.peek() {
        match ch {
            '"' => {
                chars.next(); // Consume the closing quote

                tokens.push(Token {
                    token: TokenType::StringLiteral(literal),
                    line: starting_line,
                    col: starting_col,
                });
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
    Err(LexerError::UnterminatedStringLiteral {
        line: starting_line,
        col: starting_col,
    })
}
