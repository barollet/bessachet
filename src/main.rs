#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate lazy_static;
extern crate array_init;
extern crate streaming_iterator;

/// Primitive types for the engine
#[macro_use]
pub mod types;
mod board;
mod evaluation;
mod hash_tables;
mod move_generation;
mod search;

use board::Board;

use hash_tables::*;
use types::*;

fn main() {
    let _board = Board::initial_position();

    let mut board =
        Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")
            .unwrap();

    board.search(7);
    while let Some(tt_entry) = TRANSPOSITION_TABLE.probe(board.zobrist_hasher.zobrist_key) {
        println!("{}", tt_entry.best_move);
        board.make(tt_entry.best_move);
        println!("value: {}", tt_entry.score);
        println!("eval: {}", board.evaluation());
    }
}
