#[derive(Debug)]
struct ImportantExcerpt<'a> {
    data: &'a str,
}

impl<'a> ImportantExcerpt<'a> {
    fn announce(&self, announcement: &'a str) -> &str {
        println!("ANNOUNCEMENT: {}", announcement);

        announcement
    }
}

fn main() {
    let phrase = String::from("Hello there! How are you doing!");

    let word = phrase.split('!').nth(0).expect("No '!' found!");

    let excerpt = ImportantExcerpt { data: word };

    let _ = excerpt.announce("hello there...");
}
