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

use hash_tables::*;
use utils::*;

fn main() {
    let mut board =
        Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")
            .unwrap();

    board.search(7);
    while let Some(tt_entry) = TRANSPOSITION_TABLE.probe(board.zobrist_hasher.zobrist_key) {
        println!(
            "{}",
            if board.side_to_move == Color::WHITE {
                tt_entry.best_move
            } else {
                tt_entry.best_move.transpose()
            }
        );
        let decorator = board.create_decorator();
        board.make(decorator.decorate_move(tt_entry.best_move));
        println!(
            "value: {}",
            board.material_evaluator.evaluation(Color::WHITE)
        );
    }
}
