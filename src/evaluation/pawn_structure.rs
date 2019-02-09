use board::Board;

use hash_tables::*;

use utils::*;

// Indexed by file and return adjacent files
const ISOLATION_MASK: [(BitBoard, BitBoard); 6] = [
    (file(0) | file(2), file(1)),
    (file(1) | file(3), file(2)),
    (file(2) | file(4), file(3)),
    (file(3) | file(5), file(4)),
    (file(4) | file(6), file(5)),
    (file(5) | file(7), file(6)),
];

const BACKWARD_MASK: [(BitBoard, BitBoard); 8] = [
    (file(1), file(0)),
    (file(0) | file(2), file(1)),
    (file(1) | file(3), file(2)),
    (file(2) | file(4), file(3)),
    (file(3) | file(5), file(4)),
    (file(4) | file(6), file(5)),
    (file(5) | file(7), file(6)),
    (file(6), file(7)),
];

impl Board {
    pub fn pawn_structure_evaluation(&self) -> f32 {
        let key = self.zobrist_hasher.zobrist_pawn_key;
        let side_multiplier = self.side_to_move.side_multiplier();
        if let Some(pawn_entry) = unsafe { PAWN_TABLE.probe(key) } {
            // Table hit
            side_multiplier * pawn_entry.evaluation as f32
        } else {
            // Table miss, we compute the entry and try to insert it
            let pawn_evaluation = self.doubled_pawns_score()
                + self.isolated_pawns_score()
                + self.backward_pawns_score();

            unsafe {
                PAWN_TABLE.try_insert(PawnTableEntry::new(key, pawn_evaluation));
            }

            side_multiplier * pawn_evaluation
        }
    }

    fn doubled_pawns_score(&self) -> f32 {
        let board = &self[Color::WHITE];
        -0.5 * FILES.iter().fold(0.0, |acc, file| {
            acc + (board[Piece::PAWN] & board[Color::WHITE] & *file).population() as f32 - 1.0
        }) + 0.5
            * FILES.iter().fold(0.0, |acc, file| {
                acc + (board[Piece::PAWN] & board[Color::BLACK] & *file).population() as f32 - 1.0
            })
    }

    fn isolated_pawns_score(&self) -> f32 {
        let board = &self[Color::WHITE];
        -0.5 * ISOLATION_MASK.iter().fold(0, |acc, (mask, file)| {
            acc + if (board[Piece::PAWN] & board[Color::WHITE] & *mask).population() == 0 {
                file.population()
            } else {
                0
            }
        }) as f32
            + 0.5
                * ISOLATION_MASK.iter().fold(0, |acc, (mask, file)| {
                    acc + if (board[Piece::PAWN] & board[Color::BLACK] & *mask).population() == 0 {
                        file.population()
                    } else {
                        0
                    }
                }) as f32
    }

    fn backward_pawns_score(&self) -> f32 {
        // TODO
        0.0
    }
}
