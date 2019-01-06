#![feature(reverse_bits)]

#[macro_use] extern crate enum_primitive;
#[macro_use] extern crate lazy_static;

mod board;
mod move_generation;
mod utils;

use board::Board;

use utils::*;

fn main() {
    println!("board size {}", std::mem::size_of::<Board>());
    println!("board size {}", std::mem::size_of::<Piece>());
    println!("board size {}", std::mem::size_of::<Option<Piece>>());
    println!("board size {}", std::mem::size_of::<[Option<Piece>; 64]>());

    let mut board = Board::initial_position();
    println!("zobrist {}", board.zobrist_key);
    let generator = board.create_legal_move_generator();

    for mov in generator {
        board.make(mov);
        board.unmake(mov);
    }

    println!("zobrist {}", board.zobrist_key);

    board.play_move('e', '2', 'e', '3');

    //let board = Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -").unwrap();
}

