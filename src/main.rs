mod lexer;
mod ast;
mod repl;
mod parser;
mod errors;
//use parser::{Parser};
use repl::run_repl;

fn main() {
/*
    let expression = "(1 + 2 * (3 + 2)) / -2";

    let tokens: Vec<lexer::Token> = lexer::lex(expression.to_string());
    let mut parser = Parser::new(tokens);
    let expr = parser.parse();

    println!("{}", expr);


    let value = ast::interpret(expr);

    println!("The answer is: {}", value);
*/

    run_repl();

}