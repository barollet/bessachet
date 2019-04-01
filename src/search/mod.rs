use board::Board;
use move_generation::*;

use stackvector::StackVec;

use rand::prelude::*;

use std::rc::{Rc, Weak};

// A node of the game tree
pub struct GameNode {
    // Used for scoring nodes
    score_value: i16,
    traversals: u16,
    // TODO: remove system call for dynamic allocation
    // Children and parent access
    children: Vec<Rc<GameNode>>,
    parent: Option<Weak<GameNode>>,
    // Move list that are expanded or not
    pub moves: PseudoLegalMoveList,
    number_of_selected_moves: usize,

    // Move to rollback to reach the parent node
    rollback_move: Option<ExtendedMove>,
}

// Node instanciation
// A node is instanciated with no children but the pseudo legal moves are generated and stored
impl GameNode {
    // Creates a new node from a board position (for possible moves)
    pub fn new(board: &Board) -> GameNode {
        let mut node = GameNode::default();
        node.generate_pseudo_legal_moves(board);
        node
    }

    // Creates a new node with a given parent and the move to rollback
    pub fn new_with_parent(
        board: &Board,
        parent: &Rc<GameNode>,
        ext_move: ExtendedMove,
    ) -> GameNode {
        let mut node = GameNode::new(board);
        node.set_parent(parent, ext_move);
        node
    }

    fn set_parent(&mut self, parent: &Rc<GameNode>, ext_move: ExtendedMove) {
        self.parent = Some(Rc::downgrade(parent));
        self.rollback_move = Some(ext_move);
    }

    pub fn generate_pseudo_legal_moves(&mut self, board: &Board) {
        self.moves = board.generate_pseudo_legal_moves();
    }
}

impl std::default::Default for GameNode {
    fn default() -> Self {
        GameNode {
            score_value: 0,
            traversals: 0,

            children: vec![],
            parent: None,

            moves: PseudoLegalMoveList::default(),
            number_of_selected_moves: 0,

            rollback_move: None,
        }
    }
}

impl Board {
    // Select the next node to be expanded and move the pieces accordingly
    // The first move after the children is ready to be expanded
    // TODO detect mate
    pub fn selection(&mut self) -> Rc<GameNode> {
        // Uniform selection, pure Monte Carlo
        let selected_move_index = random::<usize>() % self.game_tree_node.moves.number_of_moves;
        let number_of_children = self.game_tree_node.children.len();
        // if the move is already expanded
        if selected_move_index < number_of_children {
            // We make the move and select recursively
            let next_move = self.game_tree_node.moves[selected_move_index];

            // We try the move, if it is illegal, we select again from this node
            // otherwise we select from the selected child
            // TODO
            // We can discard the extended move because it has already been saved by previous
            // extension
            self.make(next_move);
            self.selection()
        } else {
            // if the move is not already expanded
            // we prepare the expansion by moving the move after the children

            // makes the borrow checker happy...
            Rc::get_mut(&mut self.game_tree_node)
                .unwrap()
                .moves
                .moves_list
                .swap(selected_move_index, number_of_children);

            Rc::clone(&self.game_tree_node)
        }
    }
}

impl Board {
    // Helper to check a move legality and update the game node if the move is illegal
    // Returns true iff the move is legal
    fn legal_make_and_update(&mut self, selected_move_index: usize) -> Option<ExtendedMove> {
        let pseudo_legal_move = self.game_tree_node.moves[selected_move_index];
        // If the move is legal we just do nothing (appart from making it)
        // Otherwise we put the move at the end of the valid moves and discard it (the move is already unmade)
        if let Some(ext_move) = self.legal_make(pseudo_legal_move) {
            Some(ext_move)
        } else {
            Rc::get_mut(&mut self.game_tree_node)
                .unwrap()
                .remove_from_pseudo_legal_moves(selected_move_index);
            None
        }
    }
}

impl GameNode {
    fn remove_from_pseudo_legal_moves(&mut self, selected_move_index: usize) {
        let number_of_moves = self.moves.number_of_moves;
        // swap the given move and the last valid move
        self.moves
            .moves_list
            .swap(selected_move_index, number_of_moves);

        // decrease the number of valid moves
        self.moves.number_of_moves -= 1;
    }
}

// An iterator over random moves
/*
impl<'a> Iterator for GameNode<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Move> {
        let remaining_moves = self.moves.number_of_moves - self.number_of_selected_moves;
        // We stop when we selected all the moves
        if remaining_moves == 0 {
            None
        } else {
            // We keep an invariant that the first remaining moves are not selected
            let index = random::<usize>() % remaining_moves;
            let expanded_move = self.moves.moves_list[index];
            // We restore our invariant
            self.number_of_selected_moves += 1;
            self.moves.moves_list.swap(index, remaining_moves - 1);

            Some(expanded_move)
        }
    }
}
*/
