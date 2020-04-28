mod lexer;
mod ast;
mod repl;
mod parser;
mod errors;

fn main() {
    repl::run_repl();
}