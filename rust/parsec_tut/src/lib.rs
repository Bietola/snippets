use lazy_static::lazy_static;
use regex::Regex;

/**********************/
/* Parser XML element */
/**********************/

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

/***********************/
/* Generic parser type */
/***********************/
type ParseResult<'a, T> = Result<(&'a str, T), &'a str>;

pub trait Parser<'a, T> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, T>;
}

impl<'a, F, T> Parser<'a, T> for F
where
    F: Fn(&'a str) -> ParseResult<T>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, T> {
        self(input)
    }
}

/**********************/
/* Parser combinators */
/**********************/

pub fn literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input| {
        let re = Regex::new(&format!("^{}.*", expected)).unwrap();

        if re.is_match(input) {
            Ok((&input[expected.len()..], ()))
        } else {
            Err(input)
        }
    }
}

pub fn identifier(input: &str) -> ParseResult<String> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^[a-zA-z][\w\d-]*").unwrap();
    }

    match RE.find(input) {
        Some(mtc) => {
            let mtcstr = mtc.as_str();
            Ok((&input[mtcstr.len()..], mtcstr.to_owned()))
        }
        None => Err(input),
    }
}

pub fn pair<'a, P1, P2, R1, R2>(
    parser1: P1,
    parser2: P2,
) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| match parser1.parse(input) {
        Ok((rest1, res1)) => match parser2.parse(rest1) {
            Ok((rest2, res2)) => Ok((rest2, (res1, res2))),
            Err(err) => Err(err),
        },
        Err(err) => Err(err),
    }
}

pub fn map<'a, P, F, A, B>(parser: P, fun: F) -> impl Parser<'a, B>
where
    F: Fn(A) -> B,
    P: Parser<'a, A>
{
    move |input| match parser.parse(input) {
        Ok((rest, res)) => Ok((rest, fun(res))),
        Err(err) => Err(err),
    }
}

pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_, rhs)| rhs)
}

pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(lhs, _)| lhs)
}

pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>> 
where
    P: Parser<'a, A>,
{
    move |input| {
        let mut result = Vec::new();
        let mut remaining = input;

        loop {
            match parser.parse(remaining) {
                Ok((rest, single)) => {
                    remaining = rest;
                    result.push(single);
                }
                Err(rest) => return Ok((rest, result)),
            }
        }
    }
}

#[test]
fn literal_parser() {
    let parse_joe = literal("Hello Joe!");
    assert_eq!(Ok(("", ())), parse_joe.parse("Hello Joe!"));
    assert_eq!(
        Ok((" Hello Robert!", ())),
        parse_joe.parse("Hello Joe! Hello Robert!")
    );
    assert_eq!(Err("Hello Mike!"), parse_joe.parse("Hello Mike!"));
}
#[test]
fn identifier_parser() {
    assert_eq!(
        Ok((", identifier", "hello-there".into())),
        identifier("hello-there, identifier")
    );
}

#[test]
fn pair_parser() {
    assert_eq!(
        pair(pair(literal("<"), identifier), literal(">")).parse("<hello-there>"),
        Ok(("", (((), "hello-there".into()), ()))),
    )
}

#[test]
fn right_parser() {
    let tag = right(literal("<"), identifier);

    assert_eq!(tag.parse("<hello-there"), Ok(("", "hello-there".into())),)
}

#[test]
fn left_parser() {
    let tag = left(literal("<"), identifier);

    assert_eq!(tag.parse("<hello-there"), Ok(("", ())),)
}

#[test]
fn zero_or_more_letters_a() {
    let parse_letters_a = zero_or_more(literal("a"));

    assert_eq!(
        parse_letters_a.parse("aaa"),
        Ok(("", vec![(), (), ()])),
    );

    assert_eq!(
        parse_letters_a.parse(""),
        Ok(("", vec![])),
    );
}
