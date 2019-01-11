use board::{Board, HalfBoard};
use move_generation::LegalMoveGenerator;

use utils::*;

impl HalfBoard {
    // Returns the number of pieces of the given color on this HalfBoard
    fn get_number_of(&self, piece: Piece, color: Color) -> i16 {
        (self[piece] & self[color]).0.count_ones() as i16
    }

    fn get_material_difference(&self, piece: Piece) -> i16 {
        self.get_number_of(piece, Color::WHITE) - self.get_number_of(piece, Color::BLACK)
    }
    // Returns the material evaluation from White POV of this HalfBoard
    fn material_evaluation(&self) -> f32 {
        (200 * self.get_material_difference(Piece::KING)
            + 9 * self.get_material_difference(Piece::QUEEN)
            + 5 * self.get_material_difference(Piece::ROOK)
            + 3 * (self.get_material_difference(Piece::BISHOP)
                + self.get_material_difference(Piece::KNIGHT))
            + 1 * self.get_material_difference(Piece::PAWN)) as f32
    }

    fn pawn_structure_evaluation(&self) -> f32 {
        // Doubled pawns
        // Isolated pawns
        // Blocked pawns
        0.0
    }
}

impl Board {
    // Evaluates the position of the given board
    // TODO better evaluation
    pub fn evaluation(&self, move_generator: &LegalMoveGenerator) -> f32 {
        let move_pov = &self[self.side_to_move];
        move_pov.material_evaluation()
            + move_pov.pawn_structure_evaluation()
            + self.mobility_evaluation(move_generator)
    }

    // TODO change mobility evaluation to other criteria
    fn mobility_evaluation(&self, move_generator: &LegalMoveGenerator) -> f32 {
        0.1 * (move_generator.number_of_legal_moves() as f32
            - self
                .create_opponent_legal_move_generator()
                .number_of_legal_moves() as f32)
    }
}
