mod lexer;
mod token;
mod error;
mod token_iter;

use lexer::lex;

fn main() {
    lex("examples/fib_recursive.ren", 4);
}