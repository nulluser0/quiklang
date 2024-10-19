//! # Lexer
//!
//! The lexer is responsible for converting the source code into a stream of tokens.
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

use std::{
    iter::Peekable,
    str::{Chars, FromStr},
};

use quiklang_common::{
    data_structs::tokens::{Keyword, Operator, Symbol, TokenType},
    errors::{lexer::LexerError, CompilerError, Span},
    CompilationReport,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token: TokenType,
    pub span: Span,
}

/// Custom made struct to implement line and col.
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

    fn next_char(&mut self) -> Option<char> {
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

    fn peek_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn get_position(&self) -> (usize, usize, usize) {
        (self.current_pos, self.line, self.col)
    }
}

/// Tokenizes the input source code and returns a list of tokens.
/// Errors are pushed into the `report`.
pub fn tokenize(source_code: &str, file_id: usize, report: &mut CompilationReport) -> Vec<Token> {
    let mut chars = CharStream::new(source_code);
    let mut tokens = Vec::new();

    while let Some(&c) = chars.peek_char() {
        if is_skippable(c) {
            chars.next_char();
            continue;
        }

        let (start_pos, start_line, start_col) = chars.get_position();

        match c {
            '/' => {
                // Handle comments or division
                chars.next_char(); // Consume '/'
                if let Some(&next_char) = chars.peek_char() {
                    if next_char == '/' {
                        // Single-line comment: skip until end of line
                        chars.next_char(); // Consume second '/'
                        while let Some(c) = chars.next_char() {
                            if c == '\n' {
                                break;
                            }
                        }
                        continue;
                    } else if next_char == '*' {
                        // Multi-line comment: skip until '*/'
                        chars.next_char(); // Consume '*'
                        let mut found_end = false;
                        while let Some(c) = chars.next_char() {
                            if c == '*' {
                                if let Some(&'/') = chars.peek_char() {
                                    chars.next_char(); // Consume '/'
                                    found_end = true;
                                    break;
                                }
                            }
                        }
                        if !found_end {
                            report.add_error(CompilerError::LexerError(
                                LexerError::UnterminatedMultiLineComment {
                                    span: Span::new(
                                        file_id,
                                        start_pos,
                                        chars.current_pos,
                                        start_line,
                                        start_col,
                                    ),
                                },
                            ));
                        }
                        continue;
                    } else {
                        // It's a division operator
                        tokens.push(Token {
                            token: TokenType::Operator(Operator::Divide),
                            span: Span::new(
                                file_id,
                                start_pos,
                                chars.current_pos,
                                start_line,
                                start_col,
                            ),
                        });
                        continue;
                    }
                } else {
                    // It's a division operator at EOF
                    tokens.push(Token {
                        token: TokenType::Operator(Operator::Divide),
                        span: Span::new(
                            file_id,
                            start_pos,
                            chars.current_pos,
                            start_line,
                            start_col,
                        ),
                    });
                    continue;
                }
            }
            '"' => {
                match tokenize_string_literal(
                    &mut chars, file_id, start_pos, start_line, start_col, report,
                ) {
                    Ok(token) => tokens.push(token),
                    Err(_) => {
                        // Error already reported
                    }
                }
                continue;
            }
            c if is_identifier_start(c) => {
                let identifier = lex_identifier(&mut chars);
                let end_pos = chars.current_pos;
                let _end_line = chars.line;
                let _end_col = chars.col;
                let token_type = match Keyword::from_str(&identifier) {
                    Ok(keyword) => TokenType::Keyword(keyword),
                    Err(_) => TokenType::Identifier(identifier),
                };
                tokens.push(Token {
                    token: token_type,
                    span: Span::new(file_id, start_pos, end_pos, start_line, start_col),
                });
                continue;
            }
            c if c.is_ascii_digit() => {
                match lex_number(
                    &mut chars, file_id, start_pos, start_line, start_col, report,
                ) {
                    Ok(token) => tokens.push(token),
                    Err(_) => {
                        // Error already reported
                    }
                }
                continue;
            }
            '+' | '-' | '*' | '=' | '!' | '>' | '<' | '&' | '|' | '@' | '^' | '~' | '.' => {
                match tokenize_operator_or_symbol(
                    &mut chars, file_id, start_pos, start_line, start_col, c, report,
                ) {
                    Ok(Some(token)) => tokens.push(token),
                    Ok(None) => (), // Token was already handled (e.g., comments)
                    Err(_) => {
                        // Error already reported
                    }
                }
                continue;
            }
            '(' | ')' | '{' | '}' | '[' | ']' | ',' | ';' | ':' => {
                let symbol = match c {
                    '(' => Symbol::LeftParen,
                    ')' => Symbol::RightParen,
                    '{' => Symbol::LeftBrace,
                    '}' => Symbol::RightBrace,
                    '[' => Symbol::LeftBracket,
                    ']' => Symbol::RightBracket,
                    ',' => Symbol::Comma,
                    ';' => Symbol::Semicolon,
                    ':' => Symbol::Colon,
                    _ => unreachable!(),
                };
                chars.next_char(); // Consume symbol
                tokens.push(Token {
                    token: TokenType::Symbol(symbol),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                });
                continue;
            }
            _ => {
                // Unrecognized character
                report.add_error(CompilerError::LexerError(
                    LexerError::UnrecognizedCharacter {
                        character: c,
                        span: Span::new(
                            file_id,
                            start_pos,
                            chars.current_pos,
                            start_line,
                            start_col,
                        ),
                    },
                ));
                chars.next_char(); // Skip the unrecognized character
                continue;
            }
        }
    }

    tokens.push(Token {
        token: TokenType::EOF,
        span: Span::new(
            file_id,
            chars.current_pos,
            chars.current_pos,
            chars.line,
            chars.col,
        ),
    });

    // Optional: Print tokens for debugging
    // println!("{:#?}", tokens);

    tokens
}

fn is_skippable(c: char) -> bool {
    c.is_whitespace()
}

fn is_identifier_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn lex_identifier(chars: &mut CharStream) -> String {
    let mut identifier = String::new();
    while let Some(&c) = chars.peek_char() {
        if c.is_alphanumeric() || c == '_' {
            identifier.push(c);
            chars.next_char();
        } else {
            break;
        }
    }
    identifier
}

fn lex_number(
    chars: &mut CharStream,
    file_id: usize,
    start_pos: usize,
    start_line: usize,
    start_col: usize,
    report: &mut CompilationReport,
) -> Result<Token, ()> {
    let mut number = String::new();
    let mut has_dot = false;

    while let Some(&c) = chars.peek_char() {
        if c.is_ascii_digit() {
            number.push(c);
            chars.next_char();
        } else if c == '.' && !has_dot {
            has_dot = true;
            number.push(c);
            chars.next_char();
        } else {
            break;
        }
    }

    if has_dot {
        match number.parse::<f64>() {
            Ok(value) => Ok(Token {
                token: TokenType::FloatLiteral(value),
                span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
            }),
            Err(_) => {
                report.add_error(CompilerError::LexerError(LexerError::InvalidNumberFormat {
                    invalid_string: number,
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }));
                Err(())
            }
        }
    } else {
        match number.parse::<i64>() {
            Ok(value) => Ok(Token {
                token: TokenType::IntegerLiteral(value),
                span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
            }),
            Err(_) => {
                report.add_error(CompilerError::LexerError(LexerError::InvalidNumberFormat {
                    invalid_string: number,
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }));
                Err(())
            }
        }
    }
}

fn tokenize_operator_or_symbol(
    chars: &mut CharStream,
    file_id: usize,
    start_pos: usize,
    start_line: usize,
    start_col: usize,
    current_char: char,
    report: &mut CompilationReport,
) -> Result<Option<Token>, ()> {
    let _ = report;
    match current_char {
        '!' => {
            if let Some(&'=') = chars.peek_char() {
                chars.next_char(); // Consume '='
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::NotEqual),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            } else {
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::LogicalNot),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            }
        }
        '=' => {
            if let Some(&'=') = chars.peek_char() {
                chars.next_char(); // Consume '='
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::Equal),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            } else {
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::Assign),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            }
        }
        '<' => {
            if let Some(&'=') = chars.peek_char() {
                chars.next_char(); // Consume '='
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::LessOrEqual),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            } else {
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::LessThan),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            }
        }
        '>' => {
            if let Some(&'=') = chars.peek_char() {
                chars.next_char(); // Consume '='
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::GreaterOrEqual),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            } else {
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::GreaterThan),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            }
        }
        '&' => {
            if let Some(&'&') = chars.peek_char() {
                chars.next_char(); // Consume '&'
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::And),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            } else {
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::BitwiseAnd),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            }
        }
        '|' => {
            if let Some(&'|') = chars.peek_char() {
                chars.next_char(); // Consume '|'
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::Or),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            } else {
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::BitwiseOr),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            }
        }
        '-' => {
            if let Some(&'>') = chars.peek_char() {
                chars.next_char(); // Consume '>'
                Ok(Some(Token {
                    token: TokenType::Symbol(Symbol::Arrow),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            } else {
                Ok(Some(Token {
                    token: TokenType::Operator(Operator::Subtract),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            }
        }
        '.' => {
            if let Some(&'.') = chars.peek_char() {
                chars.next_char(); // Consume second '.'
                if let Some(&'=') = chars.peek_char() {
                    chars.next_char(); // Consume '='
                    Ok(Some(Token {
                        token: TokenType::Operator(Operator::RangeInclusive),
                        span: Span::new(
                            file_id,
                            start_pos,
                            chars.current_pos,
                            start_line,
                            start_col,
                        ),
                    }))
                } else {
                    Ok(Some(Token {
                        token: TokenType::Operator(Operator::RangeExclusive),
                        span: Span::new(
                            file_id,
                            start_pos,
                            chars.current_pos,
                            start_line,
                            start_col,
                        ),
                    }))
                }
            } else {
                Ok(Some(Token {
                    token: TokenType::Symbol(Symbol::Dot),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                }))
            }
        }
        _ => {
            // Handle other operators if necessary
            Ok(None)
        }
    }
}

fn tokenize_string_literal(
    chars: &mut CharStream,
    file_id: usize,
    start_pos: usize,
    start_line: usize,
    start_col: usize,
    report: &mut CompilationReport,
) -> Result<Token, LexerError> {
    chars.next_char(); // Consume the initial quote
    let mut literal = String::new();
    // let mut current_pos = chars.current_pos;
    // let mut current_line = chars.line;
    // let mut current_col = chars.col;

    while let Some(&ch) = chars.peek_char() {
        match ch {
            '"' => {
                chars.next_char(); // Consume the closing quote
                return Ok(Token {
                    token: TokenType::StringLiteral(literal),
                    span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
                });
            }
            '{' => {
                chars.next_char(); // Consume '{'
                unimplemented!("String interpolation is not yet supported");
                // Alternatively, handle string interpolation start
            }
            '\\' => {
                chars.next_char(); // Consume '\\'
                if let Some(&escaped_char) = chars.peek_char() {
                    let escaped = match escaped_char {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '"' => '"',
                        '\\' => '\\',
                        other => other, // Treat unknown escapes literally
                    };
                    literal.push(escaped);
                    chars.next_char(); // Consume escaped character
                } else {
                    // Unterminated string literal
                    report.add_error(
                        LexerError::UnterminatedStringLiteral {
                            span: Span::new(
                                file_id,
                                start_pos,
                                chars.current_pos,
                                start_line,
                                start_col,
                            ),
                        }
                        .into(),
                    );
                    return Err(LexerError::UnterminatedStringLiteral {
                        span: Span::new(
                            file_id,
                            start_pos,
                            chars.current_pos,
                            start_line,
                            start_col,
                        ),
                    });
                }
            }
            _ => {
                literal.push(ch);
                chars.next_char();
            }
        }
    }

    // If we reach here, the string was unterminated
    report.add_error(
        LexerError::UnterminatedStringLiteral {
            span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
        }
        .into(),
    );
    Err(LexerError::UnterminatedStringLiteral {
        span: Span::new(file_id, start_pos, chars.current_pos, start_line, start_col),
    })
}
