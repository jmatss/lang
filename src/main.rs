mod error;
mod pass1;
mod pass2;

use pass1::simple_token_iter::SimpleTokenIter;
use crate::pass1::simple_token::SimpleToken;
use crate::pass2::token_iter::TokenIter;

fn main() {
    let mut simple_token_vec = Vec::new();
    let mut simple_it = SimpleTokenIter::new("examples/fib_recursive_4.ren")
        .expect("Unable to create TokenIter.");

    loop {
        let token = simple_it.next_token().unwrap();
        simple_token_vec.push(token);

        println!("{:?}", token);

        if token == SimpleToken::EndOfFile {
            break;
        }
    }

    let mut it = TokenIter::new(simple_token_vec);
}