use id::gentmp;
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
            *self.peek() == expected
        }
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
        if self.check(Token::Let) {
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
        } else {
            self.semicolon()
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
        let exp1 = self.if_expression()?;
        if self.check(Token::Semicolon) {
            self.next();
            let exp2 = self.semicolon()?;
            Ok(Syntax::Let(gentmp(Type::Unit), Type::Unit, Box::new(exp1), Box::new(exp2)))
        } else {
            Ok(exp1)
        }
    }

    fn if_expression(&mut self) -> Result<Syntax, String> {
        if self.check(Token::If) {
            self.next();
            let exp1 = self.expression()?;
            self.consume(Token::Then)?;
            let exp2 = self.expression()?;
            self.consume(Token::Else)?;
            let exp3 = self.expression()?;
            Ok(Syntax::If(Box::new(exp1), Box::new(exp2), Box::new(exp3)))
        } else {
            self.comma()
        }
    }

    fn comma(&mut self) -> Result<Syntax, String> {
        let first = self.comparison()?;
        if self.check(Token::Comma) {
            self.next();
            let mut items = Vec::new();
            items.push(Box::new(first));
            while !self.is_done() {
                let next = self.comparison()?;
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
        } else {
            Ok(first)
        }
    }

    fn comparison(&mut self) -> Result<Syntax, String> {
        let mut exp = self.addition()?;
        while !self.is_done() {
            match self.peek() {
                Token::Equal => {
                    self.next();
                    let right = self.addition()?;
                    exp = Syntax::Equal(Box::new(exp), Box::new(right))
                },
                Token::LessGreater => {
                    self.next();
                    let right = self.addition()?;
                    exp = Syntax::Not(Box::new(Syntax::Equal(Box::new(exp), Box::new(right))))
                },
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
        while !self.is_done() {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_parse_primary_expressions() {
        let cases = [
            ("true".to_string(), Ok(Syntax::Bool(true))),
            ("42".to_string(), Ok(Syntax::Int(42))),
            ("4.2".to_string(), Ok(Syntax::Float(4.2))),
            ("my_var".to_string(), Ok(Syntax::Var("my_var".to_string()))),
            ("()".to_string(), Ok(Syntax::Unit)),
            ("(true)".to_string(), Ok(Syntax::Bool(true))),
            ("(42)".to_string(), Ok(Syntax::Int(42))),
            ("(4.2)".to_string(), Ok(Syntax::Float(4.2))),
            ("(my_var)".to_string(), Ok(Syntax::Var("my_var".to_string()))),
        ];
        for (input, expected) in cases.iter() {
            let actual = parse(input);
            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn should_parse_array_create_expressions() {
        let input = "Array.make 10 0".to_string();
        let expected = Ok(Syntax::Array(
            Box::new(Syntax::Int(10)),
            Box::new(Syntax::Int(0))
        ));
        assert_eq!(parse(&input), expected);
    }

    #[test]
    fn should_parse_array_get_expressions() {
        let input = "e.(0)".to_string();
        let expected = Ok(Syntax::Get(
            Box::new(Syntax::Var("e".to_string())),
            Box::new(Syntax::Int(0))
        ));
        assert_eq!(parse(&input), expected);
    }

    #[test]
    fn should_parse_array_put_expressions() {
        let input = "e.(0) <- 42".to_string();
        let expected = Ok(Syntax::Put(
            Box::new(Syntax::Var("e".to_string())),
            Box::new(Syntax::Int(0)),
            Box::new(Syntax::Int(42)),
        ));
        assert_eq!(parse(&input), expected);
    }

    #[test]
    fn should_parse_function_application() {
        let input = "fib 5".to_string();
        let expected = Ok(Syntax::App(
            Box::new(Syntax::Var("fib".to_string())),
            vec![
                Box::new(Syntax::Int(5)),
            ]
        ));
        assert_eq!(parse(&input), expected);
    }

    #[test]
    fn should_parse_unary_operator_expressions() {
        let cases = [
            (
                "not is_valid".to_string(),
                Ok(Syntax::Not(
                    Box::new(Syntax::Var("is_valid".to_string()))
                ))
            ),

            (
                "-42".to_string(),
                Ok(Syntax::Neg(
                    Box::new(Syntax::Int(42))
                ))
            ),

            (
                "-4.2".to_string(),
                Ok(Syntax::Float(-4.2))
            ),

            (
                "-.4.2".to_string(),
                Ok(Syntax::FNeg(
                    Box::new(Syntax::Float(4.2))
                ))
            ),
        ];
        for (input, expected) in cases.iter() {
            let actual = parse(input);
            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn should_parse_arithmetic_expressions() {
        let cases = [
            (
                "3.0 *. 4.0".to_string(),
                Ok(Syntax::FMul(
                    Box::new(Syntax::Float(3.0)),
                    Box::new(Syntax::Float(4.0)),
                ))
            ),

            (
                "8.0 /. 2.0".to_string(),
                Ok(Syntax::FDiv(
                    Box::new(Syntax::Float(8.0)),
                    Box::new(Syntax::Float(2.0)),
                ))
            ),

            (
                "1 + 2".to_string(),
                Ok(Syntax::Add(
                    Box::new(Syntax::Int(1)),
                    Box::new(Syntax::Int(2)),
                ))
            ),

            (
                "2 - 1".to_string(),
                Ok(Syntax::Sub(
                    Box::new(Syntax::Int(2)),
                    Box::new(Syntax::Int(1)),
                ))
            ),

            (
                "1.0 +. 2.0".to_string(),
                Ok(Syntax::FAdd(
                    Box::new(Syntax::Float(1.0)),
                    Box::new(Syntax::Float(2.0)),
                ))
            ),

            (
                "2.0 -. 1.0".to_string(),
                Ok(Syntax::FSub(
                    Box::new(Syntax::Float(2.0)),
                    Box::new(Syntax::Float(1.0)),
                ))
            ),
        ];
        for (input, expected) in cases.iter() {
            let actual = parse(input);
            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn should_parse_comparison_expressions() {
        let cases = [
            (
                "3 = 4".to_string(),
                Ok(Syntax::Equal(
                    Box::new(Syntax::Int(3)),
                    Box::new(Syntax::Int(4)),
                ))
            ),

            (
                "3 <> 4".to_string(),
                Ok(Syntax::Not(
                    Box::new(Syntax::Equal(
                        Box::new(Syntax::Int(3)),
                        Box::new(Syntax::Int(4)),
                    ))
                ))
            ),

            (
                "3 > 4".to_string(),
                Ok(Syntax::Not(
                    Box::new(Syntax::LE(
                        Box::new(Syntax::Int(3)),
                        Box::new(Syntax::Int(4)),
                    ))
                ))
            ),

            (
                "3 >= 4".to_string(),
                Ok(Syntax::LE(
                    Box::new(Syntax::Int(4)),
                    Box::new(Syntax::Int(3)),
                ))
            ),

            (
                "3 < 4".to_string(),
                Ok(Syntax::Not(
                    Box::new(Syntax::LE(
                        Box::new(Syntax::Int(4)),
                        Box::new(Syntax::Int(3)),
                    ))
                ))
            ),

            (
                "3 <= 4".to_string(),
                Ok(Syntax::LE(
                    Box::new(Syntax::Int(3)),
                    Box::new(Syntax::Int(4)),
                ))
            ),
        ];
        for (input, expected) in cases.iter() {
            let actual = parse(input);
            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn should_parse_tuples() {
        let cases = [
            (
                "(1, 2.3)".to_string(),
                Ok(Syntax::Tuple(vec![
                    Box::new(Syntax::Int(1)),
                    Box::new(Syntax::Float(2.3)),
                ]))
            ),
        ];
        for (input, expected) in cases.iter() {
            let actual = parse(input);
            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn should_parse_if_expressions() {
        let cases = [
            (
                "if some_condition then 1 else 0".to_string(),
                Ok(Syntax::If(
                    Box::new(Syntax::Var("some_condition".to_string())),
                    Box::new(Syntax::Int(1)),
                    Box::new(Syntax::Int(0))
                ))
            ),
        ];
        for (input, expected) in cases.iter() {
            let actual = parse(input);
            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn should_parse_semicolon_separated_expressions() {
        let cases = [
            (
                "print 1; print 2".to_string(),
                Ok(Syntax::Let(
                    "Tu1".to_string(),
                    Type::Unit,
                    Box::new(Syntax::App(
                        Box::new(Syntax::Var("print".to_string())),
                        vec![Box::new(Syntax::Int(1))]
                    )),
                    Box::new(Syntax::App(
                        Box::new(Syntax::Var("print".to_string())),
                        vec![Box::new(Syntax::Int(2))]
                    )),
                ))
            ),
        ];
        for (input, expected) in cases.iter() {
            let actual = parse(input);
            assert_eq!(actual, *expected);
        }
    }

    #[test]
    fn should_parse_let_expressions() {
        let cases = [
            (
                "let x = 5 in fib x".to_string(),
                Ok(Syntax::Let(
                    "x".to_string(),
                    Type::Var(None),
                    Box::new(Syntax::Int(5)),
                    Box::new(Syntax::App(
                        Box::new(Syntax::Var("fib".to_string())),
                        vec![Box::new(Syntax::Var("x".to_string()))]
                    )),
                ))
            ),

            (
                "let rec fact x = if x = 1.0 then 1.0 else x *. (fact (x -. 1.0)) in print (fib 5)".to_string(),
                Ok(Syntax::LetRec(
                    FunDef {
                        name: ("fact".to_string(), Type::Var(None)),
                        args: vec![("x".to_string(), Type::Var(None))],
                        body: Box::new(Syntax::If(
                            Box::new(Syntax::Equal(
                                Box::new(Syntax::Var("x".to_string())),
                                Box::new(Syntax::Float(1.0))
                            )),
                            Box::new(Syntax::Float(1.0)),
                            Box::new(Syntax::FMul(
                                Box::new(Syntax::Var("x".to_string())),
                                Box::new(Syntax::App(
                                    Box::new(Syntax::Var("fact".to_string())),
                                    vec![
                                        Box::new(Syntax::FSub(
                                            Box::new(Syntax::Var("x".to_string())),
                                            Box::new(Syntax::Float(1.0))
                                        ))
                                    ]
                                ))
                            ))
                        ))
                    },
                    Box::new(Syntax::App(
                        Box::new(Syntax::Var("print".to_string())),
                        vec![
                            Box::new(Syntax::App(
                                Box::new(Syntax::Var("fib".to_string())),
                                vec![Box::new(Syntax::Int(5))]
                            ))
                        ]
                    )),
                ))
            ),

            (
                "let (x, y) = foo in bar".to_string(),
                Ok(Syntax::LetTuple(
                    vec![
                        ("x".to_string(), Type::Var(None)),
                        ("y".to_string(), Type::Var(None)),
                    ],
                    Box::new(Syntax::Var("foo".to_string())),
                    Box::new(Syntax::Var("bar".to_string()))
                ))
            ),
        ];
        for (input, expected) in cases.iter() {
            let actual = parse(input);
            assert_eq!(actual, *expected);
        }
    }
}
