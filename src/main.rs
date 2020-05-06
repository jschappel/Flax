mod lexer;
mod ast;
mod repl;
mod parser;
mod errors;
mod interpreter;
mod environment;
mod callable;

fn main() {
    repl::run_repl();
}