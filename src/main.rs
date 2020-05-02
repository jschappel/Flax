mod lexer;
mod ast;
mod repl;
mod parser;
mod errors;
mod interpreter;
mod environment;

fn main() {
    repl::run_repl();
}