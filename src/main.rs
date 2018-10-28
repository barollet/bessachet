#![feature(reverse_bits)]

mod board;
mod move_generation;
mod utils;

pub use board::Board;

pub use utils::*;

use move_generation::init_magic_tables;
use move_generation::generate_king_attacks;

fn main() {
    init_magic_tables();

    println!("board size {}", std::mem::size_of::<Board>());

    let board = Board::initial_position();

    for mov in board.knight_moves() {
        println!("{:?}", mov);
    }
    /*
    for mov in board.simple_pawn_pushs().chain(board.double_pawn_pushs()) {
        println!("{:?}", mov);
    }

    for mov in board.bishop_moves() {
        println!("{:?}", mov);
    }

    for mov in board.rook_moves() {
        println!("{:?}", mov);
    }

    for mov in board.queen_moves() {
        println!("{:?}", mov);
    }*/

    //let board = Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -").unwrap();
    //println!("{:?}", board);
    generate_king_attacks();
}

