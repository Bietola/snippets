use std::rc::*;

#[derive(Debug)]
enum List<T> {
    Cons(T, Rc<List<T>>),
    Nil,
}

macro_rules! list {
    () => { Nil };
    ( $head:expr, $($tail:expr),+ ) => { Cons($head, Rc::new(list![$($tail),*])) };
    ( $last:expr ) => { Cons($last, Rc::new(Nil)) };
}

fn main() {
    use List::{Cons, Nil};

    let lst1 = list![1, 2, 3];
    let lst2 = list![4, 5, 6];

    println!("{:?}; {:?}", lst1, lst2);

    let tail = Rc::new(list![42]);
    let lst1 = Cons(0, Rc::clone(&tail));
    let lst2 = Cons(0, Rc::clone(&tail));

    println!("1: {:?}\n2: {:?}", lst1, lst2);
}
