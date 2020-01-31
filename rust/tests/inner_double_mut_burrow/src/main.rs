use std::thread;
use std::time::Duration;

struct Dummy { data: String }

#[derive(Default)]
struct Keeper<'a> {
    tag: String,
    dummies: Vec<&'a mut Dummy>,
}

impl<'a> Keeper<'a> {
    fn dad() -> Self {
        Self { tag: "dad".into(), ..Default::default() }
    }

    fn mom() -> Self {
        Self { tag: "mom".into(), ..Default::default() }
    }

    fn keep(&mut self, dummy: &'a mut Dummy) {
        dummy.data += &self.tag;
        self.dummies.push(dummy);
    }
}

fn assign_parents<'a>(dummy: &'a mut Dummy) -> (Keeper<'a>, Keeper<'a>) {
    let mut dad = Keeper::dad();
    let mut mom = Keeper::mom();

    dad.keep(dummy);
    mom.keep(dummy);

    (dad, mom)
}

fn main() {

    println!("Hello, world!");
}
