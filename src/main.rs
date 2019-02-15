#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate lazy_static;
extern crate array_init;

mod board;
mod evaluation;
mod hash_tables;
#[macro_use]
mod move_generation;
mod search;
mod utils;

use board::Board;

use hash_tables::*;
use utils::*;

fn main() {
    let _board = Board::initial_position();

    let mut board =
        Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")
            .unwrap();

    board.search(7);
    while let Some(tt_entry) = TRANSPOSITION_TABLE.probe(board.zobrist_hasher.zobrist_key) {
        println!("{}", tt_entry.best_move);
        board.make(tt_entry.best_move);
        println!(
            "value: {}",
            board.material_evaluator.evaluation(Color::WHITE)
        );
    }
}
