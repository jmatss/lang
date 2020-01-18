mod error;
mod lexer;
mod parser;

use crate::error::CustomError;
use crate::lexer::lexer::lex;

pub type CustomResult<T> = Result<T, CustomError>;

fn main() {
    // Loop through all files and lex simple tokens...
    let simple_tokens = lex("examples/fib_iterative_4.ren").unwrap();
    for simple_token in simple_tokens {
        println!("{:?}", simple_token);
    }
    //let mut it = TokenIter::new(simple_token_vec);
}