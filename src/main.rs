use std::fs;

#[macro_use]
extern crate lazy_static;

mod options;

mod parser;

pub mod lexer;

// use crate::parser::Parser;

use options::Options;

use lexer::{Lexer, types::TokenTypes};

fn main() {
    let data = fs::read_to_string("foo.js").expect("Unable to read file");

    let mut num_of_chars = 0;

    println!("Chars are:");
    for ch in data.chars() {
        num_of_chars+=1;
        print!("{:?}, ", ch);
    }

    println!("\nNum of chars: {}", num_of_chars);

    let opts = Options::default();

    let mut lex = Lexer::new(&opts, &data);

    loop {
        lex.next_token();
        println!("{:#?} {:#?}", lex.state.kind, lex.state.value);

        if lex.state.kind == &TokenTypes::eof {
            break;
        }
    }

    // lex.next_token();
    // println!("{:#?} {:#?}", lex.state.kind, lex.state.value);

    // let mut parser = Parser::new(&data, false);

    // print!("{:?}", lex.next());

    // while let Some(token) = parser.lexer.next() {
    //     print!("{:?}, ", token);
    // }
}
