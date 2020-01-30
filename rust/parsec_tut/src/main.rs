#[macro_use] extern crate lazy_static;
extern crate regex;

mod lib;

fn main() {
    let parser = lib::match_literal("hello there");

    let res = parser("hello there! How are you doing?");

    println!("{:?}", res);
}
