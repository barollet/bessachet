#![feature(reverse_bits)]

#[macro_use] extern crate enum_primitive;

mod board;
mod move_generation;
mod utils;

use board::Board;

use utils::*;

use move_generation::init_magic_tables;

use move_generation::*;
use board::*;

fn main() {
    init_magic_tables();

    println!("Magic table loaded");

    println!("board size {}", std::mem::size_of::<Board>());
    println!("board size {}", std::mem::size_of::<Piece>());
    println!("board size {}", std::mem::size_of::<Option<Piece>>());
    println!("board size {}", std::mem::size_of::<[Option<Piece>; 64]>());

    let mut board = Board::initial_position();

    for mov in board.possible_moves() {
        println!("{}", mov);
    }

    println!("bishop attack\n{:?}", bishop_attack(Square::from_char_rank_file('e', '1'), BitBoard::empty()));
    println!("bishop attack\n{:?}", bishop_attack(Square::from_char_rank_file('e', '1'), ROW_2));
    println!("bishop attack\n{:?}", bishop_attack(Square::from_char_rank_file('e', '1'), BitBoard::new(0b1111111100011111111000000000000000000000001000101101110111111111)));

    println!("occ\n{:?}", BitBoard::new(0b1111111100011111111000000000000000000000001000101101110111111111));

    println!("bishop attack\n{:?}", bishop_attack(Square::from_char_rank_file('e', '1'), !BitBoard::empty()));
    //let board = Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -").unwrap();
    //println!("{:?}", board);
}

