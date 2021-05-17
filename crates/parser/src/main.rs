// #![allow(unused_imports)]
// #![allow(dead_code)]
// #![allow(unused_variables)]
// #![allow(non_snake_case)]
// #![allow(unused_macros)]

#[macro_use]
mod macros;
mod context;
pub mod lexer;
mod parser;
pub mod token;

use global_common::{input::StringInput, BytePos, Pos};
use parser::Parser;
pub use parser::Tokens;
use std::fs;

fn main() {
    let data = fs::read_to_string("foo.js").expect("Unable to read file");

    // let mut num_of_chars = 0;

    // println!("Chars are:");
    // for (i, ch) in data.char_indices() {
    //     println!("{}: {:?}", i, ch);
    //     num_of_chars += 1;
    // }

    // println!("\nNum of chars: {}\n", num_of_chars);

    let input = StringInput::new(&data, BytePos(0), BytePos::from_usize(data.len()));

    // let opts = Options::default();

    // let lex = Lexer::new(input);

    let mut p = Parser::new(input);

    let r = p.parse_program();

    println!("{:#?}", r);

    // for token in lex {
    //     println!("{:#?}", token.token);
    // }

    // while let Some(_) = lex.cur() {
    //     lex.next_token();
    //     // println!("{:#?}", lex.state.token_type);
    // }

    // lex.next_token();
    // println!("{:#?} {:#?}", lex.state.kind, lex.state.value);

    // let mut parser = Parser::new(&data, false);

    // print!("{:?}", lex.next());

    // while let Some(token) = parser.lexer.next() {
    //     print!("{:?}, ", token);
    // }
}
