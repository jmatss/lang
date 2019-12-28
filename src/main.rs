mod lexer;
mod token;
mod error;
mod token_iter;
mod pass1;

use pass1::token_iter::TokenIter;
//use lexer::lex;

fn main() {
    let mut it = TokenIter::new("examples/fib_recursive_4.ren")
        .expect("Unable to create TokenIter.");

    let a = it.next_token().unwrap();

    println!("{:?}", a);
    /*
    let tokens = lex("examples/fib_recursive_4.ren", 4).expect("err");
    for token in tokens {
        println!("{:?}", token);
    }
    */
}