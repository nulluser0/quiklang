use std::{process, str::FromStr};

#[derive(Debug, PartialEq)]
pub enum Token {
    // Keywords
    Keyword(Keyword),

    // Identifiers
    Identifier(String),

    // Literals
    IntegerLiteral(i64),
    StringLiteral(String),

    // Operators
    Operator(Operator),

    // Symbols/Delimiters
    Symbol(Symbol),

    // End of File
    EOF,
}

// Keywords
#[derive(Debug, PartialEq)]
pub enum Keyword {
    Import,     // import
    Fn,         // fn
    Let,        // let
    Mut,        // mut
    Return,     // return
    If,         // if
    Else,       // else
    Err,        // Err
    Ok,         // Ok
    Exit,       // exit
    Break,      // break
}

impl FromStr for Keyword {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, <Keyword as FromStr>::Err> {
        match s {
            "import" => Ok(Keyword::Import),
            "fn" => Ok(Keyword::Fn),
            "let" => Ok(Keyword::Let),
            "mut" => Ok(Keyword::Mut),
            "return" => Ok(Keyword::Return),
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "Err" => Ok(Keyword::Err),
            "Ok" => Ok(Keyword::Ok),
            "exit" => Ok(Keyword::Exit),
            "break" => Ok(Keyword::Break),
            _ => Err(format!("{} is not a keyword.", s)),
        }
    }
}

// Operators
#[derive(Debug, PartialEq)]
pub enum Operator {
    Assign,      // =
    Add,         // +
    Subtract,    // -
    Multiply,    // *
    Divide,      // /
    GreaterThan, // >>
    LessThan,    // <<
    GreaterOrEqual, // >=
    LessOrEqual,    // <=
    Equal,       // ==
    NotEqual,    // !=
    Not,         // !
    And,         // &&
    Or,          // ||
    Pipe,        // >
}

// Symbols/Delimiters
#[derive(Debug, PartialEq)]
pub enum Symbol {
    LeftParen,   // (
    RightParen,  // )
    LeftBrace,   // {
    RightBrace,  // }
    LeftBracket, // [
    RightBracket,// ]
    DataBracket, // |
    Comma,       // ,
    Semicolon,   // ;
    Colon,       // :
    Dot,         // .
    Arrow,       // ->
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
            '!' => {
                if let Some(&'=') = src.get(1) {
                    tokens.push(Token::Operator(Operator::NotEqual));
                    drain_char = false;
                    src.drain(0..2); // Remove '!=' characters
                } else {
                    tokens.push(Token::Operator(Operator::Not));
                }
            }
            '.' => tokens.push(Token::Symbol(Symbol::Dot)),
            // Operators:
            '=' => {
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
                if let Some(&'>') = src.get(1) {
                    tokens.push(Token::Symbol(Symbol::Arrow));
                    drain_char = false;
                    src.drain(0..2); // Remove '->' characters
                } else {
                    tokens.push(Token::Operator(Operator::Subtract));
                }
            }
            '*' => tokens.push(Token::Operator(Operator::Multiply)),
            '/' => {
                if let Some(&'/') = src.get(1) {
                    // It is a comment. Ignore all characters until new line.
                    src.drain(0..2);
                    while let Some(&commented_character) = src.first() {
                        if commented_character == '\n' {
                            break;
                        }
                        src.drain(0..1); // Remove the processed character
                    }
                    drain_char = false;
                } else {
                    tokens.push(Token::Operator(Operator::Divide));
                }
            }
            '>' => {
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
                if let Some(&'&') = src.get(1) {
                    tokens.push(Token::Operator(Operator::And));
                    drain_char = false;
                    src.drain(0..2); // Remove '&&' characters
                } else {
                    error_unknown_char(character);
                }
            }
            '|' => {
                if let Some(&'|') = src.get(1) {
                    tokens.push(Token::Operator(Operator::Or));
                    drain_char = false;
                    src.drain(0..2); // Remove '||' characters
                } else {
                    tokens.push(Token::Symbol(Symbol::DataBracket));
                }
            }

            // Multicharacter tokens:
            _ => {
                // Numeric literal
                if character.is_ascii_digit() {
                    let mut num: i64 = 0;
                    while let Some(&number) = src.first() {
                        if number.is_ascii_digit() {
                            // Convert the char digit to its numeric value
                            let digit_value = number.to_digit(10).unwrap() as i64;
                            num = num * 10 + digit_value;
                            src.drain(0..1); // Remove the processed character
                        } else {
                            drain_char = false;
                            break; // Break the loop if the character is not a numeric digit
                        }
                    }
                    tokens.push(Token::IntegerLiteral(num))
                // Identifier, or keyword
                } else if character.is_ascii_alphabetic() || character == '_' {
                    let mut word = String::new();
                    while let Some(&alpha) = src.first() {
                        if alpha.is_ascii_alphanumeric() || alpha == '_' {
                            word.push(alpha);
                            src.drain(0..1);
                        } else {
                            drain_char = false;
                            break;
                        }
                    }
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
                    while let Some(&c) = src.first() {
                        if c == '"' {
                            // Drain the '"' char
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
                    tokens.push(Token::StringLiteral(string_literal));
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