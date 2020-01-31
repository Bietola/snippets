struct Context<'a>(&'a str);

struct Parser<'a, 'b> {
    ctx: &'a Context<'b>,
}

#[derive(Debug)]
struct Ast;

type ParseResult<'a> = Result<Ast, &'a str>;

impl<'a> Parser<'_, 'a> {
    fn parse(&self) -> ParseResult<'a> {
        // TODO: dummy...
        Err(&self.ctx.0[1..])
    }
}

fn do_parse(ctx: Context) -> ParseResult {
    Parser { ctx: &ctx }.parse()
}

fn main() {
    let to_parse = "parse me god damn it!";
    let ctx = Context(to_parse);
    let parse_result = do_parse(ctx);

    // ctx should be dropped here

    println!("{:?}", parse_result);
}
