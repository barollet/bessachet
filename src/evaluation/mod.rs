mod material_evaluation;
mod pawn_structure;

pub use self::material_evaluation::MaterialEvaluator;

use board::Board;

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
