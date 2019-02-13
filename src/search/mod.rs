use board::Board;
use evaluation::*;
use hash_tables::*;
use move_generation::{Move, NULL_MOVE};
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

        macro_rules! quiesce_loop {
            ($macro: ident) => {
                $macro!(self do let score = -self.quiesce(-beta, -alpha) => {
                    if score >= beta {
                        return beta;
                    }
                    if score > alpha {
                        alpha = score;
                    }
                });
            }
        }
        // If check then all moves
        if self.is_king_checked() {
            quiesce_loop!(for_legal_moves_in);
        } else {
            quiesce_loop!(for_legal_captures_in);
        };

        alpha
    }

    pub fn alpha_beta(&mut self, mut alpha: f32, beta: f32, depth_left: u8) -> f32 {
        let key = self.zobrist_hasher.zobrist_key;
        // Check for TT Hit
        if let Some(tt_entry) = TRANSPOSITION_TABLE.probe(key) {
            // TT Hit
            if tt_entry.depth >= depth_left {
                return tt_entry.score;
            }
        }
        // Quiesce search to prevent from the horizon effect
        if depth_left == 0 {
            return self.quiesce(alpha, beta);
        }

        let mut best_mov = NULL_MOVE;
        let mut max_score = f32::NEG_INFINITY;
        let mut node_type = NodeType::AllNode;

        search_legal_moves!(for mov in self do let score = -self.alpha_beta(-beta, -alpha, depth_left - 1) => {
            if score > max_score {
                max_score = score;
                best_mov = mov;
            }

            if score >= beta {
                // Write result in TT
                TRANSPOSITION_TABLE.try_insert(&TTReadableEntry::new(
                    key,
                    mov, // Refutation move
                    depth_left,
                    score,
                    NodeType::CutNode,
                ));
                return beta;
            }
            if score > alpha {
                node_type = NodeType::PVNode;
                alpha = score;
            }
        });

        // Write result in TT
        // Alpha move or PV move
        // No moves means mate
        if best_mov == NULL_MOVE {
            TRANSPOSITION_TABLE.try_insert(&TTReadableEntry::new(
                key,
                NULL_MOVE,
                u8::max_value(),
                MATE_SCORE,
                NodeType::CutNode,
            ));

            MATE_SCORE
        } else {
            // Otherwise we insert the PV node from here
            TRANSPOSITION_TABLE.try_insert(&TTReadableEntry::new(
                key, best_mov, depth_left, max_score, node_type,
            ));

            alpha
        }
    }

    pub fn search(&mut self, depth: u8) {
        for dep in 1..=depth {
            self.alpha_beta(f32::NEG_INFINITY, f32::INFINITY, dep);
        }
    }
}
