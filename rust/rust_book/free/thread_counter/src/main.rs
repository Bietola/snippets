use std::thread;
use std::sync::mpsc;
use std::sync::Arc;
use std::iter;

const N: usize = 10;

fn exchange<T: Copy>(old: T, new: T) -> T {
    use std::mem::swap;
    swap(old, new);
    new
}

fn main() {
    let (tx, rx) = mpsc::channel();
    let v = Arc::new({
        let mut c = 0;
        iter::repeat_with(|| exchange(c, c + 1))
            .take(N)
            .collect::<Vec<u64>>()
    });

    let mut handles = vec![];
    for i in 0..N {
        let tx = mpsc::Sender::clone(&tx);
        let v  = Arc::clone(&v);
        handles.push(
            thread::spawn(move || {
                tx.send(Some((i, v[i] * 2))).unwrap();
            })
        );
    }
    {
        let tx = mpsc::Sender::clone(&tx);
        thread::spawn(move || {
            for handle in handles {
                handle.join().unwrap();
            }
            tx.send(None).unwrap();
        });
    }

    let mut result: Vec<u64> = iter::repeat(0).take(N).collect();

    for msg in rx {
        match msg {
            None => break,
            Some((i, res)) => result[i] = res,
        }
    }

    println!("Cacluation over: {:?}", result);
}
