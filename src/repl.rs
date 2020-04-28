use std::io;
use std::io::Write;

use crate::lexer;
use crate::interpreter;
use crate::parser;
use parser::{Parser};
use colored::*;

pub fn run_repl() {
    println!("{}", "Welcome to Flax! v0.1".purple());
    let mut mode = ReplMode::Normal;
    loop {
        let mut buffer = String::new();
        print!(">>>");
        io::stdout().flush().expect("Unable to flush buffer");
        let _stdin = io::stdin().read_line(&mut buffer).unwrap();
        let buffer = buffer.trim();

        // Evaluate the command
        match buffer {
            ":quit" => {
                println!("{}", "Goodbye".purple());
                break; 
            },
            ":debug" => {
                mode = ReplMode::Debug;
                println!("{}", "Now in debug mode".yellow());
            },
            ":normal" => {
                mode = ReplMode::Normal;
                println!("{}", "Now in normal mode".yellow());
            },
            _ => evaluate(buffer, &mode),
        }       
    }
}

fn evaluate(stmt: &str, repl_mode: &ReplMode) {
    match repl_mode {
        ReplMode::Normal => parse_statement(stmt),
        ReplMode::Debug => debug_parse_statement(stmt),
    }
}


fn parse_statement(stmt: &str) {
    match lexer::lex_line(stmt.to_string()) {
        Ok(tokens) => {
            let mut parser = Parser::new(tokens);
            match parser.parse() {
                Ok(expr) => {
                    let value = interpreter::interpret_ast(expr);
                    match value {
                        Ok(value) => println!("{}", value),
                        Err(e) => println!("{}", e.to_string().red()),
                    }
                },
                Err(e) => println!("{}", e.to_string().red()),
            }
        },
        Err(e) => println!("{}", e.to_string().red()),
    }
}

// Debug mode prints the ast
fn debug_parse_statement(stmt: &str) {
    match lexer::lex_line(stmt.to_string()) {
        Ok(tokens) => {
            let mut parser = Parser::new(tokens);
            match parser.parse() {
                Ok(expr) => {
                    println!("{}", expr);
                },
                Err(e) => println!("{}", e.to_string().red()),
            }
        },
        Err(e) => println!("{}", e.to_string().red()),
    }
}

#[derive(PartialEq)]
enum ReplMode {
    Normal, Debug
}

// **IMPORTANT **

//use std::env;
// Reading command line args!
//let args: Vec<String> = env::args().collect();
//println!("{:?}", args);