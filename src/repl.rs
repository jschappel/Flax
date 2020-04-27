use std::io;
use std::io::Write;

use crate::lexer;
use crate::ast;
use crate::parser;
use parser::{Parser};


pub fn run_repl() {
    println!("Welcome to Flax!");

    loop {
        let mut buffer = String::new();
        print!(">>>");
        io::stdout().flush().expect("Unable to flush buffer");
        let _stdin = io::stdin().read_line(&mut buffer).unwrap();
        let buffer = buffer.trim();

        // Check if we need to quit
        if quit(buffer) { 
            println!("Goodbye");
            break; 
        }

        parse_statement(buffer);

    }
}

fn quit(stmt: &str) -> bool {
    match stmt {
        ":quit" => true,
        _ => false
    }
}


fn parse_statement(stmt: &str) {
    let tokens: Vec<lexer::Token> = lexer::lex_line(stmt.to_string());
    let mut parser = Parser::new(tokens);
    let expr = parser.parse();
    let value = ast::interpret2(expr);

    println!("{}", value);
}


// **IMPORTANT **

//use std::env;
// Reading command line args!
//let args: Vec<String> = env::args().collect();
//println!("{:?}", args);