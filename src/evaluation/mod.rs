mod material_evaluation;
mod pawn_structure;

pub use self::material_evaluation::MaterialEvaluator;

use board::Board;
use types::Color;

pub const MATE_SCORE: f32 = -1000.0;

impl Board {
    // Evaluates the position of the given board
    // TODO better evaluation
    pub fn evaluation(&self) -> f32 {
        self.material_evaluator
            .evaluation(self.position.side_to_move)
            + self.pawn_structure_evaluation()
    }
}

pub fn side_multiplier(color: Color) -> f32 {
    [-1.0, 1.0][color as usize]
}
