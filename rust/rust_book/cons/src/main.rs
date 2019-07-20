#[derive(Debug)]
enum List {
    Cons(i32, Box<List>),
    Nil,
}

fn main() {
    use List::{Cons, Nil};

    let list: List = Cons(1, Box::new(Cons(2, Box::new(Nil))));

    println!("{:?}", list);
}
