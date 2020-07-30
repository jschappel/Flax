use crate::lexer;
use crate::interpreter;
use crate::parser;
use crate::errors::ComplierError;

use std::io;
use std::io::Write;
use std::env;
use parser::{Parser};
use interpreter::Interpreter;
use colored::*;

type ReplResult = std::result::Result<(), ComplierError>;


pub fn run_repl() {
    //Check if REPL was run with args
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        if args.contains(&"--debug".to_owned()) {
            let path = &args[1];
            if let Err(error) = print_ast(path) {
                println!("{}", error.to_string().red());
            }
            std::process::exit(0);
        } else {
            let path = &args[1];
            if let Err(error) = interpret_file(path) {
                println!("{}", error.to_string().red());
            }
            std::process::exit(0);
        }   
    }

    println!("{}", "Welcome to Flax! v0.1".purple());
    let mut mode = ReplMode::Normal;
    let mut interpreter = Interpreter::new();

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
            _ => {
                if let Err(e) = evaluate(buffer, &mode, &mut interpreter) {
                    println!("{}", e.to_string().red());
                }
            },
        }       
    }
}

fn evaluate(stmt: &str, repl_mode: &ReplMode, interpreter: &mut Interpreter) -> ReplResult {
    match repl_mode {
        ReplMode::Normal => interpret_stmt(stmt, interpreter),
        ReplMode::Debug => debug_parse_expr(stmt),
    }
}


fn interpret_stmt(stmt: &str, interpreter: &mut Interpreter) -> ReplResult {
    let tokens = lexer::lex_line(stmt.to_string())?;
    let ast = Parser::new(tokens).parse()?;
    interpreter.interpret(ast)?;
    Ok(())
}

fn interpret_file(filename: &str) -> ReplResult {
    let tokens = lexer::lex_file(filename)?;
    let ast = Parser::new(tokens).parse()?;
    interpreter::Interpreter::new().interpret(ast)?;
    Ok(())
}

/// Prints the ast that the parser produces for a expression
// TODO: Remove
fn debug_parse_expr(input: &str) -> ReplResult {
    let tokens = lexer::lex_line(input.to_string())?;
    let expr = Parser::new(tokens).parse_expression()?;
    println!("{}", expr);
    Ok(())
}

/// Prints the ast that the parser produces for a file
fn print_ast(path: &str) -> ReplResult {
    let tokens = lexer::lex_file(path)?;
    let ast = Parser::new(tokens).parse()?;
    println!("{:#?}", ast);
    Ok(())
}

#[derive(PartialEq)]
enum ReplMode {
    Normal, Debug
}