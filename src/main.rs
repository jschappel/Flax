mod lexer;
mod ast;
mod repl;
mod parser;
mod errors;
mod interpreter;
mod environment;
mod callable;
mod native_functions;
mod strlib;

fn main() {

    repl::run_repl();
}