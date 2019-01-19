use board::Board;
use move_generation::{ExtendedMove, NULL_EXTMOVE};
use std::f32;

/* Basic sequential alpha beta implementation */
impl Board {
    // Performs a quiesce search (ie stops when there is no tactical moves)
    fn quiesce(&mut self, mut alpha: f32, beta: f32) -> f32 {
        let stand_pat = self.evaluation();
        if stand_pat >= beta {
            return beta;
        }
        if alpha < stand_pat {
            alpha = stand_pat;
        }

        // If check then all moves
        // TODO factor this (maybe in a macro)
        let mut move_generator = self.create_legal_move_generator();
        if move_generator.is_king_checked() {
            for mov in move_generator {
                self.make(mov);
                let score = -self.quiesce(-beta, -alpha);
                self.unmake(mov);

                if score >= beta {
                    return beta;
                }
                if score > alpha {
                    alpha = score;
                }
            }
        } else {
            for capture in move_generator.capture_iterator() {
                self.make(capture);
                let score = -self.quiesce(-beta, -alpha);
                self.unmake(capture);

                if score >= beta {
                    return beta;
                }
                if score > alpha {
                    alpha = score;
                }
            }
        };

        alpha
    }

    pub fn alpha_beta(&mut self, mut alpha: f32, beta: f32, depth_left: u8) -> f32 {
        // Quiesce search to prevent from the horizon effect
        if depth_left == 0 {
            return self.quiesce(alpha, beta);
        }
        let move_generator = self.create_legal_move_generator();
        for mov in move_generator {
            self.make(mov);
            let score = -self.alpha_beta(-beta, -alpha, depth_left - 1);
            self.unmake(mov);

            if score >= beta {
                return beta;
            }
            if score > alpha {
                alpha = score;
            }
        }

        alpha
    }

    pub fn best_move(&mut self, depth: u8) -> (ExtendedMove, f32) {
        let move_generator = self.create_legal_move_generator();
        let mut max_score = f32::NEG_INFINITY;
        let mut best_move = NULL_EXTMOVE;
        for mov in move_generator {
            self.make(mov);
            let score = -self.alpha_beta(f32::NEG_INFINITY, f32::INFINITY, depth);
            self.unmake(mov);

            if score > max_score {
                max_score = score;
                best_move = mov;
            }
        }

        (best_move, max_score)
    }
}
