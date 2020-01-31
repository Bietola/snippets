macro_rules! list {
    () => { Nil };
    ( $head:expr, $($tail:expr),+ ) => { Cons($head, Box::new(list!($($tail),*))) };
    ( $head:expr ) => { Cons( $head, Box::new(Nil) ) };
}

#[derive(Debug)]
enum List<T> {
    Cons(T, Box<List<T>>),
    Nil,
}

fn main() {
    use List::{Cons, Nil};

    let list = list![1, 2, 3];

    println!("{:?}", list);
}
