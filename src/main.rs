#![feature(reverse_bits)]

#[macro_use] extern crate enum_primitive;
#[macro_use] extern crate lazy_static;

mod board;
mod move_generation;
mod utils;

use board::Board;

use utils::*;

use move_generation::*;
use board::*;

fn main() {
    println!("board size {}", std::mem::size_of::<Board>());
    println!("board size {}", std::mem::size_of::<Piece>());
    println!("board size {}", std::mem::size_of::<Option<Piece>>());
    println!("board size {}", std::mem::size_of::<[Option<Piece>; 64]>());

    let mut board = Board::initial_position();
    let mut generator = board.create_legal_move_generator();

    let mov = board.get_move('e', '2', 'e', '3');
    board.make(mov);
    let mov1 = board.get_move('g', '1', 'h', '3');
    board.make(mov1);
    println!("{}", board);
    let mov = board.get_move('f', '1', 'a', '6');
    println!("{:?}", mov);
    board.make(mov);
    println!("{}", board);
    board.unmake(mov);
    println!("{}", board);
    board.unmake(mov1);
    println!("{}", board);

    //let board = Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -").unwrap();
}

