pub mod material_evaluation;
mod pawn_structure;

pub use self::material_evaluation::MaterialEvaluator;

use board::Board;

impl Board {
    // Evaluates the position of the given board
    // TODO better evaluation
    pub fn evaluation(&self) -> f32 {
        self.material_evaluator.evaluation(self.side_to_move) + self.pawn_structure_evaluation()
    }
}
