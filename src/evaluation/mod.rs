pub mod material_evaluation;

pub use self::material_evaluation::MaterialEvaluator;

use board::{Board, HalfBoard};
use move_generation::LegalMoveGenerator;

use hash_tables::*;

use utils::*;

const ISOLATION_MASK: [(BitBoard, BitBoard); 6] = [
    (BitBoard(file(0) | file(2)), BitBoard(file(1))),
    (BitBoard(file(1) | file(3)), BitBoard(file(2))),
    (BitBoard(file(2) | file(4)), BitBoard(file(3))),
    (BitBoard(file(3) | file(5)), BitBoard(file(4))),
    (BitBoard(file(4) | file(6)), BitBoard(file(5))),
    (BitBoard(file(5) | file(7)), BitBoard(file(6))),
];

impl HalfBoard {
    fn doubled_pawns(&self, color: Color) -> f32 {
        -0.5 * FILES.iter().fold(0.0, |acc, file| {
            acc + (self[Piece::PAWN] & self[color] & *file).population() as f32 - 1.0
        })
    }

    fn isolated_pawns(&self, color: Color) -> f32 {
        -0.5 * ISOLATION_MASK.iter().fold(0, |acc, (mask, file)| {
            acc + if (self[Piece::PAWN] & self[color] & *mask).population() == 0 {
                file.population()
            } else {
                0
            }
        }) as f32
    }

    fn pawn_structure_evaluation(&self) -> f32 {
        // Doubled pawns
        let doubled_pawns_score =
            self.doubled_pawns(Color::WHITE) - self.doubled_pawns(Color::BLACK);
        // Isolated pawns
        let isolated_pawns_score =
            self.isolated_pawns(Color::WHITE) - self.isolated_pawns(Color::BLACK);

        // Backward pawns
        // TODO

        doubled_pawns_score + isolated_pawns_score
    }
}

impl LegalMoveGenerator {
    pub fn mobility_evaluation(color: Color) -> f32 {
        0.0
    }
}

impl Board {
    // Evaluates the position of the given board
    // TODO better evaluation
    pub fn evaluation(&self) -> f32 {
        let move_pov = &self[self.side_to_move];
        // Pawn table hit
        let pawn_eval =
            if let Some(pawn_entry) = PAWN_TABLE.probe(self.zobrist_hasher.zobrist_pawn_key) {
                pawn_entry.evaluation as f32 * [-1.0, 1.0][self.side_to_move as usize]
            } else {
                move_pov.pawn_structure_evaluation()
            };
        self.material_evaluator.evaluation(self.side_to_move) as f32
            + move_pov.pawn_structure_evaluation()
            + self.mobility_evaluation()
    }

    // TODO change mobility evaluation to other criteria
    fn mobility_evaluation(&self) -> f32 {
        LegalMoveGenerator::mobility_evaluation(self.side_to_move)
            - LegalMoveGenerator::mobility_evaluation(self.side_to_move.transpose())
    }
}
