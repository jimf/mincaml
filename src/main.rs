pub mod id;
pub mod lexer;
pub mod parser;
pub mod syntax;
pub mod token;
pub mod types;

use std::io::{self, BufRead, Write};
use parser::parse;
// use lexer::tokenize;

fn main() {
    let stdin = io::stdin();

    loop {
        print!(">> ");
        io::stdout().flush().expect("Error flushing stdout");

        let mut line = String::new();
        stdin.lock().read_line(&mut line).expect("Error reading from stdin");

        // match tokenize(&mut line) {
        //     Ok(tokens) => {
        //         println!("{:?}", tokens);
        //     },
        //     Err(msg) => {
        //         println!("{:?}", msg);
        //     }
        // }
        match parse(&mut line) {
            Ok(ast) => {
                println!("{:?}", ast);
            },
            Err(msg) => {
                println!("{}", msg);
            }
        }
    }
}
