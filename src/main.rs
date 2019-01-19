#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate lazy_static;

mod board;
mod evaluation;
mod hash_tables;
mod move_generation;
mod search;
mod utils;

use board::Board;

use utils::*;

fn main() {
    let mut board =
        Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")
            .unwrap();

    let mut other_board = board.clone();
    other_board.play_move("g2h3");
    other_board.play_move("b4c3");
    println!("eval other {}", other_board.evaluation());

    println!("eval {}", board.evaluation());
    println!("{}", board);
    let (best_mov, score) = board.best_move(3);
    println!("best move and score: {} {}", best_mov.get_raw_move(), score);
}
