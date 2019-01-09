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
    fn material_evaluation(&self) -> i16 {
        200 * self.get_material_difference(Piece::KING)
        + 9 * self.get_material_difference(Piece::QUEEN)
        + 5 * self.get_material_difference(Piece::ROOK)
        + 3 * (self.get_material_difference(Piece::BISHOP) + self.get_material_difference(Piece::KNIGHT))
        + 1 * self.get_material_difference(Piece::PAWN)
    }

    fn pawn_structure_evaluation(&self) -> i16 {
        0
    }
}

impl Board {
    // Evaluates the position of the given board
    // TODO better evaluation
    pub fn evaluation(&self, move_generator: &LegalMoveGenerator) -> i16 {
        0
    }
}
