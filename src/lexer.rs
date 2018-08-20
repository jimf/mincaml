use token::Token;

use std::iter::Peekable;
use std::str::Chars;

pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut lexer = Lexer::new(&input);
    loop {
        let token = lexer.next();
        match token {
            Token::Illegal => {
                return Err("Parse error".to_string())
            },
            Token::Eof => {
                return Ok(tokens)
            },
            _ => {
                tokens.push(token);
            }
        }
    }
}

enum NumberParseState {
    Integer,
    WithDecimal,
    BeginWithExp,
    BeginWithSignedExp,
    WithExp,
    NoNextState,
}

impl NumberParseState {
    fn next_state(&self, c: char) -> NumberParseState {
        match self {
            NumberParseState::Integer => {
                match c {
                    '0'...'9' => NumberParseState::Integer,
                    '.' => NumberParseState::WithDecimal,
                    'e' | 'E' => NumberParseState::BeginWithExp,
                    _ => NumberParseState::NoNextState,
                }
            },
            NumberParseState::WithDecimal => {
                match c {
                    '0'...'9' => NumberParseState::WithDecimal,
                    'e' | 'E' => NumberParseState::BeginWithExp,
                    _ => NumberParseState::NoNextState,
                }
            },
            NumberParseState::BeginWithExp => {
                match c {
                    '0'...'9' => NumberParseState::WithExp,
                    '-' | '+' => NumberParseState::BeginWithSignedExp,
                    _ => NumberParseState::NoNextState,
                }
            },
            NumberParseState::BeginWithSignedExp => {
                match c {
                    '0'...'9' => NumberParseState::WithExp,
                    _ => NumberParseState::NoNextState,
                }
            },
            NumberParseState::WithExp => {
                match c {
                    '0'...'9' => NumberParseState::WithExp,
                    _ => NumberParseState::NoNextState,
                }
            },
            _ => NumberParseState::NoNextState,
        }
    }
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer { input: input.chars().peekable() }
    }

    fn read(&mut self) -> Option<char> {
        self.input.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek() {
            match c {
                ' ' | '\t' | '\n' | '\r' => { self.read(); },
                _ => { break; }
            }
        }
    }

    fn skip_comment(&mut self) {
        let mut start_closing = false;
        while let Some(&c) = self.peek() {
            match c {
                '*' => { start_closing = true; },
                ')' => {
                    if start_closing {
                        self.read();
                        break;
                    } else {
                        start_closing = false;
                    }
                },
                _ => { start_closing = false; }
            }
            self.read();
        }
    }

    fn read_identifier(&mut self, first: char) -> String {
        let mut ident = String::new();
        ident.push(first);
        while let Some(&c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                ident.push(c);
                self.read();
            } else {
                break;
            }
        }
        ident
    }

    fn read_array_create(&mut self) -> String {
        let mut create = "rray.create".chars();
        let mut make = "rray.make".chars();
        let mut result = String::from("A");
        while let Some(&c) = self.peek() {
            let next_create = create.next();
            let next_make = make.next();
            if (next_create.is_some() && next_create.unwrap() == c) || (next_make.is_some() && next_make.unwrap() == c) {
                self.read();
                result.push(c);
            } else {
                break;
            }
        }
        result
    }

    fn tokenize_number(&mut self, first: char) -> Token {
        let mut result = String::new();
        result.push(first);
        let mut state = NumberParseState::Integer;
        while let Some(&c) = self.peek() {
            let next_state = state.next_state(c);
            match next_state {
                NumberParseState::NoNextState => {
                    break;
                },
                _ => {
                    result.push(c);
                    self.read();
                    state = next_state;
                }
            }
        }
        match state {
            NumberParseState::Integer => Token::Int(result),
            NumberParseState::WithDecimal | NumberParseState::WithExp => Token::Float(result),
            _ => Token::Illegal
        }
    }

    pub fn next(&mut self) -> Token {
        self.skip_whitespace();

        match self.read() {
            Some('(') => {
                match self.peek() {
                    Some('*') => {
                        self.read();
                        self.skip_comment();
                        self.next()
                    },
                    _ => Token::LParen
                }
            },
            Some(')') => Token::RParen,
            Some('-') => {
                match self.peek() {
                    Some('.') => {
                        self.read();
                        Token::MinusDot
                    },
                    _ => Token::Minus
                }
            },
            Some('+') => {
                match self.peek() {
                    Some('.') => {
                        self.read();
                        Token::PlusDot
                    },
                    _ => Token::Plus
                }
            },
            Some('*') => {
                match self.peek() {
                    Some('.') => {
                        self.read();
                        Token::AstDot
                    },
                    _ => Token::Illegal
                }
            },
            Some('/') => {
                match self.peek() {
                    Some('.') => {
                        self.read();
                        Token::SlashDot
                    },
                    _ => Token::Illegal
                }
            },
            Some('<') => {
                match self.peek() {
                    Some('>') => {
                        self.read();
                        Token::LessGreater
                    },
                    Some('=') => {
                        self.read();
                        Token::LessEqual
                    },
                    Some('-') => {
                        self.read();
                        Token::LessMinus
                    },
                    _ => Token::Less
                }
            },
            Some('=') => Token::Equal,
            Some('>') => {
                match self.peek() {
                    Some('=') => {
                        self.read();
                        Token::GreaterEqual
                    },
                    _ => Token::Greater
                }
            },
            Some(',') => Token::Comma,
            Some('A') => {
                let lit = self.read_array_create();
                if lit == "Array.create" || lit == "Array.make" {
                    Token::ArrayCreate
                } else {
                    Token::Illegal
                }
            },
            Some('.') => Token::Dot,
            Some(';') => Token::Semicolon,
            Some(ch @ _) => {
                if ch.is_ascii_lowercase() {
                    Token::from_literal(&self.read_identifier(ch))
                } else if ch.is_numeric() {
                    self.tokenize_number(ch)
                } else {
                    Token::Illegal
                }
            },
            None => Token::Eof
        }
    }
}
