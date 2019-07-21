use std::thread;

fn main() {
    let v = vec![1, 2, 3, 4];

    // I was forced to add "move" by the compiler...
    let handle = thread::spawn(move || {
        for ele in &v{
            println!("{}", *ele);
        }
    });

    handle.join().unwrap();

    // for ele in &v {
    //     println!("{}", *ele);
    // }
}
