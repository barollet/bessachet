mod board;
mod move_generation;
mod utils;

pub use board::Board;

pub use utils::*;

use move_generation::init_magic_tables;

fn main() {
    init_magic_tables();

    println!("board size {}", std::mem::size_of::<Board>());

    let board = Board::initial_position();

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
    }
}

