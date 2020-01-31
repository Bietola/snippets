fn longest_string<'a>(lhs: &'a str, rhs: &'a str) -> &'a str {
    if lhs.len() > rhs.len() {
        lhs
    } else {
        rhs
    }
}

fn main() {
    let long_s = "heloooooooooooo!";

    let longest;
    {
        let s = String::from("hello?");
        longest = longest_string(s.as_str(), long_s);

        // This works
        println!("{:?}", longest);
    };

    // This doesn't...
    // println!("{:?}", longest);
}
