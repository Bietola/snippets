trait Gen {}

trait SubGen {}

trait Spec {}

impl<T: SubGen> Gen for T {}

impl<T: Spec> SubGen for T {}

struct SubGenThing<T: SubGen>(T);

struct SpecThing<T: Spec>(T);

fn test<T>(_lhs: SubGenThing<T>, _rhs: SpecThing<T>, _other: T)
    where T: Spec
{
    unimplemented!();
}

fn main() {
    println!("Hello, world!");
}
