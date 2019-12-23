use std::fs::File;
use std::io::{BufReader, BufRead};
use crate::token::Token;
use crate::error::CustomError;
use crate::error::CustomError::LexError;
use crate::token_iter::TokenIter;

pub type CustomResult<T> = Result<T, CustomError>;

const BUF_SIZE: usize = 1 << 16;

pub fn lex(filename: &str, indent_size: usize) -> CustomResult<()> {
    let tokens: Vec<Token> = Vec::new();

    let file = File::open(filename).expect("Unable to open file.");
    let mut reader = BufReader::new(file);
    let mut token_iter = TokenIter::new();

    for line_result in reader.lines() {
        if let Ok(line) = line_result {
            token_iter.new_line(&line);
            println!("{:?}", lex_priv(token_iter, indent_size)?);
            break;
        }
    }

    Ok(())
}

fn lex_priv(mut it: TokenIter, indent_size: usize) -> CustomResult<()> {
    let current_indent_size = it.trim();
    if current_indent_size % indent_size != 0 {
        return Err(LexError("Incorrect indent size."));
    }
    let mut id = it.next_identifier()?;

    let token = Token::lookup(&id)?;
    loop {
        match token {
            Token::Function => lex_function(&mut it),
            Token::Class => lex_function(&mut it),   // TODO:
            _ => panic!("Incorrect token match.")
        }
    }
}

fn lex_function(mut it: &mut TokenIter) -> CustomResult<()> {
    let id = it.next_identifier()?;
    let arguments = it.next_arguments()?;
}