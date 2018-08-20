#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Bool(bool),
    Not,
    Int(String),
    Float(String),
    Minus,
    Plus,
    MinusDot,
    PlusDot,
    AstDot,
    SlashDot,
    Equal,
    LessGreater,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    If,
    Then,
    Else,
    Let,
    In,
    Rec,
    Comma,
    Ident(String),
    ArrayCreate,
    Dot,
    LessMinus,
    Semicolon,
    Eof,
    Illegal,
}

impl Token {
    pub fn from_literal(ident: &str) -> Token {
        match ident {
            "not" => Token::Not,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "let" => Token::Let,
            "in" => Token::In,
            "rec" => Token::Rec,
            _ => Token::Ident(ident.to_string())
        }
    }
}
