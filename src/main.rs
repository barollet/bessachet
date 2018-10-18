mod move_generation;

use move_generation::MagicEntry;

fn main() {
    let x = &[1, 2, 3];
    for i in x {
        println!("{} {}", i, x[2]);
    }
    println!("Hello, world!");

    println!("{}", std::mem::size_of::<MagicEntry>());
}
