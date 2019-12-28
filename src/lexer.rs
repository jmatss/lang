use std::fs::File;
use std::io::{BufReader, BufRead};
use crate::token::*;
use crate::error::CustomError;
use crate::error::CustomError::LexError;
//use crate::token_iter::TokenIter;

pub type CustomResult<T> = Result<T, CustomError>;
/*
pub fn lex(filename: &str, indent_size: usize) -> CustomResult<Vec<Token>> {
    let mut tokens: Vec<Token> = Vec::new();

    let file = File::open(filename).expect("Unable to open file.");
    let reader = BufReader::new(file);

    let mut i = 0;
    for line_result in reader.lines() {
        if let Ok(line) = line_result {
            println!("STEP {} ---", i);
            lex_priv(&mut tokens, line, indent_size)?;
            for token in &tokens {
                println!(" {:?}", token);
            }
        }

        tokens.push(Token::Control(Control::NewLine));
        i += 1;
    }

    Ok(tokens)
}

fn lex_priv(mut tokens: &mut Vec<Token>, line: String, indent_size: usize) -> CustomResult<()> {
    let mut it = TokenIter::new(&line);

    let current_indent_size = it.trim();
    if current_indent_size % indent_size != 0 {
        return Err(LexError("Incorrect indent size."));
    }
    tokens.push(Token::Control(Control::WhiteSpace(current_indent_size)));

    if let Some(val) = tokens.get(tokens.len() - 1) {
        println!("*{:?}", val);
    }

    let token = Token::lookup(&it.next_identifier()?)?;
    tokens.push(token.clone());

    if let Some(val) = tokens.get(tokens.len() - 1) {
        println!("*{:?}", val);
    }

    match token {
        Token::Header(Header::Function) => lex_function(&mut tokens, &mut it)?,
        Token::Header(Header::Class) => lex_function(&mut tokens, &mut it)?,   // TODO:
        Token::Header(Header::If) => lex_if(&mut tokens, &mut it)?,
        Token::Keyword(Keyword::Return) => lex_if(&mut tokens, &mut it)?,
        _ => return Err(LexError("Incorrect token match in lex_priv."))
    }



    tokens.push(Token::Control(Control::NewLine));

    Ok(())
}

fn lex_function(tokens: &mut Vec<Token>, it: &mut TokenIter) -> CustomResult<()> {
    let name = it.next_identifier()?;
    tokens.push(Token::Name(name));

    let arguments = it.next_arguments()?;
    for argument in arguments.into_iter() {
        tokens.push(argument);
    }

    let return_type = it.next_return_type()?;
    tokens.push(return_type);


    if let Some(_) = it.trim_peek()
    // TODO: Check if there are other chars after the headerEnd
    if it.trim_peek()? == ':' {
        tokens.push(Token::Header(Header::HeaderEnd));
        Ok(())
    } else {
        Err(LexError("No HeaderEnd at end of function header."))
    }
}

fn lex_if(tokens: &mut Vec<Token>, it: &mut TokenIter) -> CustomResult<()> {
    let name = it.next_identifier()?;
    tokens.push(Token::Name(name));

    let arguments = it.next_arguments()?;
    for argument in arguments.into_iter() {
        tokens.push(argument);
    }

    if it.trim_peek()? == ':' {
        tokens.push(Token::Header(Header::HeaderEnd));
        Ok(())
    } else {
        Err(LexError("No HeaderEnd at end of function header."))
    }
}
*/