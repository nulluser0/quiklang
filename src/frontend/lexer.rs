// Lexer

use std::{process, str::FromStr};

#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
            _ => Err(format!("{} is not a keyword.", s)),
        }
    }
}

// Operators
#[derive(Debug, PartialEq)]
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
    And,            // &&
    Or,             // ||
    Pipe,           // >
    Modulus,        // %
    BitwiseNot,     // ~
}

// Symbols/Delimiters
#[derive(Debug, PartialEq)]
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

fn error_unknown_char(err: char) -> ! {
    println!("Unrecognized character found in source: {:#?}", err);
    process::exit(1);
}

#[inline]
fn is_skippable(src: char) -> bool {
    src == ' ' || src == '\n' || src == '\t' || src == '\r'
}

pub fn tokenize(source_code: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = source_code.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            // Symbols:
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
            '!' => {
                chars.next();
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::Operator(Operator::NotEqual));
                } else {
                    tokens.push(Token::Operator(Operator::LogicalNot));
                }
                continue;
            }
            '=' => {
                chars.next();
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::Operator(Operator::Equal));
                } else {
                    tokens.push(Token::Operator(Operator::Assign));
                }
                continue;
            }
            '-' => {
                chars.next();
                if chars.peek() == Some(&'>') {
                    chars.next();
                    tokens.push(Token::Symbol(Symbol::Arrow));
                } else {
                    tokens.push(Token::Operator(Operator::Subtract));
                }
                continue;
            }
            '>' => {
                chars.next();
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::Operator(Operator::GreaterOrEqual));
                } else if chars.peek() == Some(&'>') {
                    chars.next();
                    tokens.push(Token::Operator(Operator::GreaterThan));
                } else {
                    tokens.push(Token::Operator(Operator::Pipe));
                }
                continue;
            }
            '<' => {
                chars.next();
                if chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::Operator(Operator::LessOrEqual));
                } else if chars.peek() == Some(&'<') {
                    chars.next();
                    tokens.push(Token::Operator(Operator::LessThan));
                } else {
                    error_unknown_char(c);
                }
                continue;
            }
            '&' => {
                chars.next();
                if chars.peek() == Some(&'&') {
                    chars.next();
                    tokens.push(Token::Operator(Operator::And));
                } else {
                    error_unknown_char(c);
                }
                continue;
            }
            '|' => {
                chars.next();
                if chars.peek() == Some(&'|') {
                    chars.next();
                    tokens.push(Token::Operator(Operator::Or));
                } else {
                    tokens.push(Token::Symbol(Symbol::DataBracket));
                }
                continue;
            }

            // Multicharacter tokens:
            _ => {
                if c.is_ascii_digit() {
                    let mut num_str = String::new();
                    let mut is_float = false;

                    while let Some(&digit) = chars.peek() {
                        if digit.is_ascii_digit() {
                            num_str.push(digit);
                        } else if digit == '.' && !is_float {
                            is_float = true;
                            num_str.push(digit);
                        } else {
                            break;
                        }
                        chars.next();
                    }

                    if is_float {
                        let float_val: f64 = num_str.parse().unwrap();
                        tokens.push(Token::FloatLiteral(float_val));
                    } else {
                        let int_val: i64 = num_str.parse().unwrap();
                        tokens.push(Token::IntegerLiteral(int_val));
                    }
                    continue;
                }

                if c.is_ascii_alphabetic() || c == '_' {
                    let mut word = String::new();
                    while let Some(&alpha) = chars.peek() {
                        if alpha.is_ascii_alphanumeric() || alpha == '_' {
                            word.push(alpha);
                            chars.next();
                        } else {
                            break;
                        }
                    }

                    match Keyword::from_str(&word) {
                        Ok(keyword) => tokens.push(Token::Keyword(keyword)),
                        Err(_) => tokens.push(Token::Identifier(word)),
                    }
                    continue;
                }

                if c == '"' {
                    chars.next();
                    let mut string_literal = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch == '"' {
                            break;
                        } else if ch == '\\' {
                            chars.next();
                            if let Some(&next_ch) = chars.peek() {
                                if next_ch == '"' {
                                    string_literal.push('"');
                                    chars.next();
                                } else {
                                    string_literal.push('\\');
                                    string_literal.push(next_ch);
                                    chars.next();
                                }
                            }
                        } else {
                            string_literal.push(ch);
                            chars.next();
                        }
                    }
                    chars.next(); // Consume closing quote
                    tokens.push(Token::StringLiteral(string_literal));
                    continue;
                }

                if c == '/' {
                    chars.next();
                    if chars.peek() == Some(&'/') {
                        chars.next();
                        while let Some(&commented_character) = chars.peek() {
                            if commented_character == '\n' {
                                break;
                            } else {
                                chars.next();
                            }
                        }
                    } else if chars.peek() == Some(&'*') {
                        chars.next();
                        while let Some(&commented_character) = chars.peek() {
                            if commented_character == '*' {
                                chars.next();
                                if chars.peek() == Some(&'/') {
                                    chars.next();
                                    break;
                                }
                            } else {
                                chars.next();
                            }
                        }
                    } else {
                        tokens.push(Token::Operator(Operator::Divide));
                    }
                    continue;
                }

                if !is_skippable(c) {
                    error_unknown_char(c);
                }
            }
        }
        chars.next(); // Consume the current character
    }
    tokens.push(Token::EOF);

    tokens
}
