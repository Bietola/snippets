use std::thread;
use std::time::Duration;

fn main() {
    let handle = thread::spawn(|| {
        for i in 0..10 {
            println!("Hello {} from the spawned thread", i);
            thread::sleep(Duration::from_millis(100));
        }
    });

    handle.join().unwrap();

    for i in 0..10 {
        println!("Hello {} from the main thread", i);
        thread::sleep(Duration::from_millis(100));
    }

    println!("Hello, world!");
}
