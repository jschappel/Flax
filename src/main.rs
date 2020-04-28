mod lexer;
mod ast;
mod repl;
mod parser;
mod errors;
mod interpreter;

fn main() {
    repl::run_repl();
}