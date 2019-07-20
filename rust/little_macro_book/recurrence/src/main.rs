macro_rules! recurrence {
    ( a[n]: $ele_type:ty = $($inits:expr),+ , ... , $recur:expr ) => {
        println!("hello!");
    };
}

fn fib() -> FibItr {
    FibItr {
        mem: [1, 1],
        pos: 0,
    }
}

struct FibItr {
    mem: [u64; 2],
    pos: usize,
}

impl Iterator for FibItr {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        self.pos += 1;

        Some(
            match self.pos - 1 {
                j if j < 2 => self.mem[j],
                _ => {
                    let tmp = self.mem[1];
                    self.mem[1] += self.mem[0];
                    self.mem[0] = tmp;
                    self.mem[1]
                }
            }
        )
    }
} 

fn main() {
    let fib = recurrence![a[n]: u64 = 0, 1, ..., a[n - 1] + a[n - 2]];

    for i in fib().take(10) {
        println!("{}", i);
    }
}
