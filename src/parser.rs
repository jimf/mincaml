use lexer::tokenize;
use syntax::{Syntax, FunDef};
use token::Token;
use types::Type;

pub fn parse(input: &str) -> Result<Syntax, String> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(&tokens);
    parser.program()
}

struct Parser<'a> {
    tokens: &'a Vec<Token>,
    position: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &Vec<Token>) -> Parser {
        Parser {
            tokens: &tokens,
            position: 0,
        }
    }

    fn peek(&mut self) -> &'a Token {
        &self.tokens[self.position]
    }

    fn previous(&mut self) -> &'a Token {
        &self.tokens[self.position - 1]
    }

    fn next(&mut self) -> &'a Token {
        if !self.is_done() {
            self.position += 1;
        }
        self.previous()
    }

    fn check(&mut self, expected: Token) -> bool {
        if self.is_done() {
            false
        } else {
            *self.next() == expected
        }
    }

    fn match_token(&mut self, expected: Vec<Token>) -> bool {
        for token in expected {
            if self.check(token) {
                self.next();
                return true
            }
        }
        false
    }

    fn consume(&mut self, expected: Token) -> Result<(), String> {
        if *self.next() != expected {
            Err(format!("Expected '{:?}'", expected))
        } else {
            Ok(())
        }
    }

    fn is_done(&mut self) -> bool {
        self.position == self.tokens.len()
    }

    // Grammar:
    //   expression      → equality
    //   equality        → comparison ( ( "<>" | "=" ) comparison )*
    //   comparison      → addition ( ( ">" | ">=" | "<" | "<=" ) addition )*
    //   addition        → multiplication ( ( "+" | "-" | "+." | "-." ) multiplication )*
    //   multiplication  → unary ( ( "/." | "*." ) unary)*
    //   unary           → ( "not" | "-" ) unary
    //                   | primary
    //   primary         → Int | Float | Bool | Ident | "(" expression ")

    fn program(&mut self) -> Result<Syntax, String> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Syntax, String> {
        self.let_expression()
    }

    fn let_expression(&mut self) -> Result<Syntax, String> {
        match self.peek() {
            Token::Let => {
                self.next();
                match self.peek() {
                    Token::Ident(ident) => {
                        self.next();
                        self.consume(Token::Equal)?;
                        let exp1 = self.expression()?;
                        self.consume(Token::In)?;
                        let exp2 = self.expression()?;
                        Ok(Syntax::Let(ident.to_string(), Type::Var(None), Box::new(exp1), Box::new(exp2)))
                    },
                    Token::Rec => {
                        self.next();
                        let exp1 = self.fundef()?;
                        self.consume(Token::In)?;
                        let exp2 = self.expression()?;
                        Ok(Syntax::LetRec(exp1, Box::new(exp2)))
                    },
                    Token::LParen => {
                        self.next();
                        let exp1 = self.pat()?;
                        self.consume(Token::RParen)?;
                        self.consume(Token::Equal)?;
                        let exp2 = self.expression()?;
                        self.consume(Token::In)?;
                        let exp3 = self.expression()?;
                        Ok(Syntax::LetTuple(exp1, Box::new(exp2), Box::new(exp3)))
                    },
                    _ => Err("Expected an identifier or token 'rec'".to_string())
                }
            },
            _ => self.semicolon()
        }
    }

    fn pat(&mut self) -> Result<Vec<(String, Type)>, String> {
        let mut result = Vec::new();
        loop {
            match self.next() {
                Token::Ident(ident) => {
                    result.push((ident.to_string(), Type::Var(None)));
                },
                _ => {
                    return Err("Expected an identifier".to_string())
                }
            }
            match self.peek() {
                Token::Comma => {
                    self.next();
                },
                _ => {
                    break;
                }
            }
        }
        Ok(result)
    }

    fn fundef(&mut self) -> Result<FunDef, String> {
        match self.peek() {
            Token::Ident(ident) => {
                self.next();
                let args = self.formal_args()?;
                self.consume(Token::Equal)?;
                let body = self.expression()?;
                Ok(FunDef {
                    name: (ident.to_string(), Type::Var(None)),
                    args: args,
                    body: Box::new(body),
                })
            },
            _ => {
                Err("Expected an identifier".to_string())
            }
        }
    }

    fn formal_args(&mut self) -> Result<Vec<(String, Type)>, String> {
        let mut args = Vec::new();
        loop {
            match self.peek() {
                Token::Ident(ident) => {
                    self.next();
                    args.push((ident.to_string(), Type::Var(None)));
                },
                _ => {
                    break;
                }
            }
        }
        if args.len() > 0 {
            Ok(args)
        } else {
            Err("Expected at least one identifier for let rec".to_string())
        }
    }

    fn semicolon(&mut self) -> Result<Syntax, String> {
        self.if_expression()
    }

    fn if_expression(&mut self) -> Result<Syntax, String> {
        match self.peek() {
            Token::If => {
                self.next();
                let exp1 = self.expression()?;
                self.consume(Token::Then)?;
                let exp2 = self.expression()?;
                self.consume(Token::Else)?;
                let exp3 = self.expression()?;
                Ok(Syntax::If(Box::new(exp1), Box::new(exp2), Box::new(exp3)))
            },
            _ => self.comma()
        }
    }

    fn comma(&mut self) -> Result<Syntax, String> {
        let first = self.equality()?;
        if self.is_done() {
            return Ok(first)
        }
        match self.peek() {
            Token::Comma => {
                self.next();
                let mut items = Vec::new();
                items.push(Box::new(first));
                while !self.is_done() {
                    let next = self.equality()?;
                    items.push(Box::new(next));
                    match self.peek() {
                        Token::Comma => {
                            self.next();
                        },
                        _ => {
                            break;
                        }
                    }
                }
                Ok(Syntax::Tuple(items))
            },
            _ => Ok(first)
        }
    }

    fn equality(&mut self) -> Result<Syntax, String> {
        let mut exp = self.comparison()?;
        while !self.is_done() {
            match self.peek() {
                Token::Equal => {
                    self.next();
                    let right = self.comparison()?;
                    exp = Syntax::Equal(Box::new(exp), Box::new(right))
                },
                Token::LessGreater => {
                    self.next();
                    let right = self.comparison()?;
                    exp = Syntax::Not(Box::new(Syntax::Equal(Box::new(exp), Box::new(right))))
                },
                _ => {
                    break;
                }
            }
        }
        Ok(exp)
    }

    fn comparison(&mut self) -> Result<Syntax, String> {
        let mut exp = self.addition()?;
        while !self.is_done() {
            match self.peek() {
                Token::Greater => {
                    self.next();
                    let right = self.addition()?;
                    exp = Syntax::Not(Box::new(Syntax::LE(Box::new(exp), Box::new(right))))
                },
                Token::GreaterEqual => {
                    self.next();
                    let right = self.addition()?;
                    exp = Syntax::LE(Box::new(right), Box::new(exp))
                },
                Token::Less => {
                    self.next();
                    let right = self.addition()?;
                    exp = Syntax::Not(Box::new(Syntax::LE(Box::new(right), Box::new(exp))))
                },
                Token::LessEqual => {
                    self.next();
                    let right = self.addition()?;
                    exp = Syntax::LE(Box::new(exp), Box::new(right))
                },
                _ => {
                    break;
                }
            }
        }
        Ok(exp)
    }

    fn addition(&mut self) -> Result<Syntax, String> {
        let mut exp = self.multiplication()?;
        while !self.is_done() {
            match self.peek() {
                Token::Plus => {
                    self.next();
                    let right = self.multiplication()?;
                    exp = Syntax::Add(Box::new(exp), Box::new(right))
                },
                Token::Minus => {
                    self.next();
                    let right = self.multiplication()?;
                    exp = Syntax::Sub(Box::new(exp), Box::new(right))
                },
                Token::PlusDot => {
                    self.next();
                    let right = self.multiplication()?;
                    exp = Syntax::FAdd(Box::new(exp), Box::new(right))
                },
                Token::MinusDot => {
                    self.next();
                    let right = self.multiplication()?;
                    exp = Syntax::FSub(Box::new(exp), Box::new(right))
                },
                _ => {
                    break;
                }
            }
        }
        Ok(exp)
    }

    fn multiplication(&mut self) -> Result<Syntax, String> {
        let mut exp = self.unary()?;
        while !self.is_done() {
            match self.peek() {
                Token::AstDot => {
                    self.next();
                    let right = self.unary()?;
                    exp = Syntax::FMul(Box::new(exp), Box::new(right))
                },
                Token::SlashDot => {
                    self.next();
                    let right = self.unary()?;
                    exp = Syntax::FDiv(Box::new(exp), Box::new(right))
                },
                _ => {
                    break;
                }
            }
        }
        Ok(exp)
    }

    fn unary(&mut self) -> Result<Syntax, String> {
        match self.peek() {
            Token::Not => {
                self.next();
                let exp = self.unary()?;
                Ok(Syntax::Not(Box::new(exp)))
            },
            Token::Minus => {
                self.next();
                let exp = self.unary()?;
                match exp {
                    Syntax::Float(f) => Ok(Syntax::Float(-f)),
                    _ => Ok(Syntax::Neg(Box::new(exp)))
                }
            },
            Token::MinusDot => {
                self.next();
                let exp = self.unary()?;
                Ok(Syntax::FNeg(Box::new(exp)))
            },
            _ => self.application()
        }
    }

    fn application(&mut self) -> Result<Syntax, String> {
        let exp = self.primary()?;
        let mut args = Vec::new();
        loop {
            if self.is_done() {
                break;
            }
            match self.primary() {
                Ok(prim) => {
                    args.push(Box::new(prim));
                },
                Err(_) => {
                    break;
                }
            }
        }
        if args.len() > 0 {
            Ok(Syntax::App(Box::new(exp), args))
        } else {
            Ok(exp)
        }
    }

    fn primary(&mut self) -> Result<Syntax, String> {
        let result = match self.peek() {
            Token::Bool(b) => {
                self.next();
                Ok(Syntax::Bool(*b))
            },
            Token::Int(i) => {
                self.next();
                Ok(Syntax::Int(i.parse().unwrap()))
            },
            Token::Float(f) => {
                self.next();
                Ok(Syntax::Float(f.parse().unwrap()))
            },
            Token::Ident(i) => {
                self.next();
                Ok(Syntax::Var(i.to_string()))
            },
            Token::LParen => {
                self.next();
                match *self.peek() {
                    Token::RParen => {
                        self.next();
                        Ok(Syntax::Unit)
                    },
                    _ => {
                        let exp = self.expression();
                        self.consume(Token::RParen)?;
                        exp
                    }
                }
            },
            Token::ArrayCreate => {
                self.next();
                let exp1 = self.primary()?;
                let exp2 = self.primary()?;
                Ok(Syntax::Array(Box::new(exp1), Box::new(exp2)))
            },
            _ => Err("Parse error apple".to_string())
        };
        if !self.is_done() && *self.peek() == Token::Dot {
            let exp1 = result?;
            self.next();
            self.consume(Token::LParen)?;
            let exp2 = self.expression()?;
            self.consume(Token::RParen)?;
            if !self.is_done() && *self.peek() == Token::LessMinus {
                self.next();
                let exp3 = self.expression()?;
                Ok(Syntax::Put(Box::new(exp1), Box::new(exp2), Box::new(exp3)))
            } else {
                Ok(Syntax::Get(Box::new(exp1), Box::new(exp2)))
            }
        } else {
            result
        }
    }
}
