use std::sync::mpsc;
use std::thread;

fn main() {
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        tx.send("hi :3").unwrap();
    });

    println!("received message!: {}", rx.recv().unwrap());
}
