use std::ops::Add;

#[derive(Debug, Clone, Copy)]
struct Num(i32);
#[derive(Debug)]
struct Str<'a>(&'a str);

impl Add<Str<'_>> for Num {
    type Output = String;

    fn add(self, other: Str) -> Self::Output {
        let mut result = self.0.to_string();
        result.push_str(other.0);
        result
    }
}

fn main() {
    let my_2 = Num(2);
    println!("{}", my_2 + Str(" times the love"));
    println!("still here: {:?}", my_2);
}
