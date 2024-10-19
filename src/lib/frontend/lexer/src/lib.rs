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

use std::{iter::Peekable, str::Chars};

use quiklang_common::{
    data_structs::tokens::{Token, TokenType},
    errors::{LexerError, Span},
};

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
