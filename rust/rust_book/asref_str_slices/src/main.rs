fn plus_2<T: AsRef<str>>(slice: &[T]) -> Vec<String> {
    slice.iter().map(|s| s.as_ref().to_owned() + "2").collect()
}

fn main() {
    let slice = ["hello", "how", "are", "you"];

    let slice = plus_2(&slice[1..]);

    let slice = plus_2(&slice[1..]);

    println!("{:?}", slice);
}
