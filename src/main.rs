mod lexer;
mod ast;
mod repl;
mod parser;
mod errors;
//use parser::{Parser};
use repl::run_repl;

fn main() {
    run_repl();
}