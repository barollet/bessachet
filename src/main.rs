mod move_generation;

use move_generation::MagicEntry;
use move_generation::init_magic_tables;
use move_generation::rook_attack;

fn main() {
    let x = &[1, 2, 3];
    for i in x {
        println!("{} {}", i, x[2]);
    }
    println!("Hello, world!");

    println!("{}", std::mem::size_of::<MagicEntry>());

    init_magic_tables();

    print_bitboard_mask(rook_attack(0, 0));
    print_bitboard_mask(rook_attack(0, 2));
    print_bitboard_mask(rook_attack(51, 2));

    /*
    print_bitboard_mask(rook_black_mask(0));
    print_bitboard_mask(rook_key(0, 0));
    print_bitboard_mask(rook_key(0, 64));
    print_bitboard_mask(rook_attack(0, 64));
    print_bitboard_mask(rook_attack(0, 32));
    print_bitboard_mask(rook_attack(0, 2));
    */
}

#[allow(dead_code)]
fn print_bitboard_mask(u: u64) {
    for i in (0..8).rev() {
        let line: u8 = ((u >> (8*i)) & 0xff) as u8;
        for j in 0..8 {
            print!("{}", (line >> j) & 0x1);
        }
        println!();
    }
    println!();
}
