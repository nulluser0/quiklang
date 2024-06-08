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

fn is_skippable(src: char) -> bool {
    src == ' ' || src == '\n' || src == '\t' || src == '\r'
}

pub fn tokenize(source_code: String) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut src: Vec<char> = source_code.chars().collect();

    while !src.is_empty() {
        let character = src[0];
        let mut drain_char = true;
        match character {
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
            '~' => tokens.push(Token::Operator(Operator::BitwiseNot)),
            '!' => {
                if src.is_empty() {
                    break;
                }
                if let Some(&'=') = src.get(1) {
                    tokens.push(Token::Operator(Operator::NotEqual));
                    drain_char = false;
                    src.drain(0..2); // Remove '!=' characters
                } else {
                    tokens.push(Token::Operator(Operator::LogicalNot));
                }
            }
            '.' => tokens.push(Token::Symbol(Symbol::Dot)),
            // Operators:
            '=' => {
                if src.is_empty() {
                    break;
                }
                if let Some(&'=') = src.get(1) {
                    tokens.push(Token::Operator(Operator::Equal));
                    drain_char = false;
                    src.drain(0..2); // Remove '==' characters
                } else {
                    tokens.push(Token::Operator(Operator::Assign));
                }
            }
            '+' => tokens.push(Token::Operator(Operator::Add)),
            '-' => {
                if src.is_empty() {
                    break;
                }
                if let Some(&'>') = src.get(1) {
                    tokens.push(Token::Symbol(Symbol::Arrow));
                    drain_char = false;
                    src.drain(0..2); // Remove '->' characters
                } else {
                    tokens.push(Token::Operator(Operator::Subtract));
                }
            }
            '*' => tokens.push(Token::Operator(Operator::Multiply)),
            '>' => {
                if src.is_empty() {
                    break;
                }
                if let Some(&'>') = src.get(1) {
                    tokens.push(Token::Operator(Operator::GreaterThan));
                    drain_char = false;
                    src.drain(0..2); // Remove '>>' characters
                } else if let Some(&'=') = src.get(1) {
                    tokens.push(Token::Operator(Operator::GreaterOrEqual));
                    drain_char = false;
                    src.drain(0..2); // Remove '>=' characters
                } else {
                    tokens.push(Token::Operator(Operator::Pipe));
                }
            }
            '<' => {
                if src.is_empty() {
                    break;
                }
                if let Some(&'<') = src.get(1) {
                    tokens.push(Token::Operator(Operator::LessThan));
                    drain_char = false;
                    src.drain(0..2); // Remove '<<' characters
                } else if let Some(&'=') = src.get(1) {
                    tokens.push(Token::Operator(Operator::LessOrEqual));
                    drain_char = false;
                    src.drain(0..2); // Remove '<=' characters
                } else {
                    error_unknown_char(character);
                }
            }
            '&' => {
                if src.is_empty() {
                    break;
                }
                if let Some(&'&') = src.get(1) {
                    tokens.push(Token::Operator(Operator::And));
                    drain_char = false;
                    src.drain(0..2); // Remove '&&' characters
                } else {
                    error_unknown_char(character);
                }
            }
            '|' => {
                if src.is_empty() {
                    break;
                }
                if let Some(&'|') = src.get(1) {
                    tokens.push(Token::Operator(Operator::Or));
                    drain_char = false;
                    src.drain(0..2); // Remove '||' characters
                } else {
                    tokens.push(Token::Symbol(Symbol::DataBracket));
                }
            }
            '%' => tokens.push(Token::Operator(Operator::Modulus)),

            // Multicharacter tokens:
            _ => {
                // Numeric literal
                if character.is_ascii_digit() {
                    let mut num: i64 = 0;
                    let mut decimal_num: f64 = 0.0;
                    let mut decimal_place: f64 = 1.0;
                    let mut is_decimal = false;

                    while !src.is_empty() {
                        if let Some(&number) = src.first() {
                            if number.is_ascii_digit() {
                                // Convert the char digit to its numeric value
                                let digit_value = number.to_digit(10).unwrap() as i64;
                                if is_decimal {
                                    decimal_place *= 0.1;
                                    decimal_num += digit_value as f64 * decimal_place;
                                } else {
                                    num = num * 10 + digit_value;
                                }
                                src.drain(0..1); // Remove the processed character
                            } else if number == '.' && !is_decimal {
                                // Handle the decimal point
                                is_decimal = true;
                                src.drain(0..1); // Remove the decimal point
                            } else {
                                break; // Break the loop if the character is not a numeric digit or a single decimal point
                            }
                        }
                    }

                    drain_char = false;

                    if is_decimal {
                        let final_num = num as f64 + decimal_num;
                        tokens.push(Token::FloatLiteral(final_num));
                    } else {
                        tokens.push(Token::IntegerLiteral(num));
                    }
                } else if character.is_ascii_alphabetic() || character == '_' {
                    let mut word = String::new();
                    while !src.is_empty() {
                        if let Some(&alpha) = src.first() {
                            if alpha.is_ascii_alphanumeric() || alpha == '_' {
                                word.push(alpha);
                                src.drain(0..1);
                            } else {
                                break;
                            }
                        }
                    }
                    drain_char = false;
                    // Before pushing word, ensure check for reserved keywords.
                    match Keyword::from_str(&word) {
                        Ok(keyword) => tokens.push(Token::Keyword(keyword)),
                        Err(_) => tokens.push(Token::Identifier(word)),
                    }
                // String literal
                } else if character == '"' {
                    // Handle string literals
                    let mut string_literal = String::new();
                    src.drain(0..1); // Remove the opening quote
                    while !src.is_empty() {
                        if let Some(&c) = src.first() {
                            if c == '"' {
                                break;
                            } else if c == '\\' && Some(&'"') == src.get(1) {
                                src.drain(0..1);
                                string_literal.push(c);
                                src.drain(0..1);
                            } else {
                                string_literal.push(c);
                                src.drain(0..1);
                            }
                        }
                    }
                    if src.is_empty() {
                        drain_char = false;
                    }
                    tokens.push(Token::StringLiteral(string_literal));
                } else if character == '/' {
                    if let Some(&'/') = src.get(1) {
                        // It is a comment. Ignore all characters until new line.
                        src.drain(0..2);
                        while !src.is_empty() {
                            if let Some(&commented_character) = src.first() {
                                if commented_character == '\n' {
                                    break;
                                } else {
                                    src.drain(0..1); // Remove the processed character
                                }
                            }
                        }
                        drain_char = false;
                    } else if let Some(&'*') = src.get(1) {
                        // It is a multiline comment. Ignore all characters until '*/'.
                        src.drain(0..2);
                        while !src.is_empty() {
                            if let Some(&commented_character) = src.first() {
                                if commented_character == '*' {
                                    src.drain(0..1);
                                    if !src.is_empty() && Some(&'/') == src.get(1) {
                                        src.drain(0..1);
                                        break;
                                    }
                                } else {
                                    src.drain(0..1); // Remove the processed character
                                }
                            }
                        }
                        drain_char = false;
                    } else {
                        tokens.push(Token::Operator(Operator::Divide));
                    }
                } else if !is_skippable(character) {
                    error_unknown_char(character);
                }
            }
        }
        if drain_char {
            src.drain(0..1); // Remove processed character from the source
        }
    }
    tokens.push(Token::EOF);

    tokens
}
