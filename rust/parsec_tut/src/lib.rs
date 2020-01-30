use lazy_static::lazy_static;
use regex::Regex;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}
pub fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
    move |input| {
        let re = Regex::new(&format!("^{}.*", expected)).unwrap();

        if re.is_match(input) {
            Ok((&input[expected.len()..], ()))
        } else {
            Err(input)
        }
    }
}

pub fn identifier(input: &str) -> Result<(String, &str), &str> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^[-_a-zA-Z]*").unwrap();
    }

    match RE.find(input) {
        Some(mtc) => {
            let mtcstr = mtc.as_str();
            Ok((mtcstr.to_owned(), &input[mtcstr.len()..]))
        }
        None => Err(input),
    }
}

#[test]
fn literal_parser() {
    let parse_joe = match_literal("Hello Joe!");
    assert_eq!(Ok(("", ())), parse_joe("Hello Joe!"));
    assert_eq!(
        Ok((" Hello Robert!", ())),
        parse_joe("Hello Joe! Hello Robert!")
    );
    assert_eq!(Err("Hello Mike!"), parse_joe("Hello Mike!"));
}

#[test]
fn identifier_parser() {
    assert_eq!(
        Ok(("hello-there".into(), ", identifier")),
        identifier("hello-there, identifier")
    );
}
