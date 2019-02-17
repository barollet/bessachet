use board::prelude::*;
use evaluation::side_multiplier;
use std::ops::{Index, IndexMut};
use types::*;

// (3*3*3*2*9)^2 = 486^2 = 236,196 which is about 1/4 MB
// NOTE: We use H.G. Muller's material index with a constant offset to avoid negative index
// See: http://www.talkchess.com/forum3/viewtopic.php?t=33561
// TODO change this to fixed point values to add minor advantages
//  2*243*nWR - (2*243-81)*nBR + (243+27+9)*nWB - (243+27)*nBB + (243+3)*nWN - (243+1)*nBN + 729*nWQ + 1458*nBQ + 2916*nWP + 26244*nBP
//  constant offset to avoid negative index: 1838
//const MATERIAL_TABLE_SIZE: usize = 237_490;
const MATERIAL_TABLE_SIZE: usize = 239_327;
lazy_static! {
    static ref MATERIAL_TABLE: [i8; MATERIAL_TABLE_SIZE] = pre_compute_material_table();
}

const PIECE_MATERIAL_VALUES: [i8; 6] = [
    3,   // Knight
    3,   // Bishop
    5,   // Rook
    9,   // Queen
    1,   // Pawn
    100, // King // NOTE: should not be used
];

#[derive(Copy, Clone)]
pub struct MaterialEvaluator {
    table_index: isize, // NOTE: This should never be negative

    populations: PieceColorOffsets,
    valid_index: bool, // TODO make this valid again after an invalid index
}

const INDEX_OFFSETS: PieceColorOffsets = PieceColorOffsets([
    [
        -(243 + 1),      // Black knight
        -(243 + 27),     // Black bishop
        -(2 * 243 - 81), // Black rook
        1458,            // Black queen
        26244,           // Black pawn
    ],
    [
        3 + 243,      // White knight
        243 + 27 + 9, // White bishop
        2 * 243,      // White rook
        729,          // White queen
        2916,         // White pawn
    ],
]);

// Iterates over all the possible material constellations (without promotions) and pre compute the
// material evaluation
// TODO add insufficient material for draws
fn pre_compute_material_table() -> [i8; MATERIAL_TABLE_SIZE] {
    let mut material_table = [0; MATERIAL_TABLE_SIZE];
    let mut material_evaluator = MaterialEvaluator::initialize(&Position::empty_board());

    macro_rules! for_piece {
        ($piece:expr, $max_number:expr, $body:expr) => {
            for _i in 0..=$max_number {
                material_evaluator.uncapture_piece($piece, WHITE);
                for _i in 0..=$max_number {
                    material_evaluator.uncapture_piece($piece, BLACK);
                    $body;
                    material_evaluator.capture_piece($piece, BLACK);
                }
                material_evaluator.capture_piece($piece, WHITE);
            }
        };
    }

    macro_rules! unnest_for_calls {
        (($piece: expr, $max_number: expr)) => {
            for_piece!($piece, $max_number, material_table[material_evaluator.table_index as usize] =
                        material_value(&material_evaluator.populations))
        };
        (($piece: expr, $max_number: expr), $($others:tt)*) => {
            for_piece!($piece, $max_number, unnest_for_calls!($($others)*));
        };
    }

    unnest_for_calls!(
        (Piece::ROOK, 2),
        (Piece::BISHOP, 2),
        (Piece::KNIGHT, 2),
        (Piece::QUEEN, 1),
        (Piece::PAWN, 8)
    );

    material_table
}

// Computes the material value for the given piece constellation
// TODO: add something for bishop pair for example
fn material_value(piece_populations: &PieceColorOffsets) -> i8 {
    piece_populations[WHITE]
        .iter()
        .zip(&PIECE_MATERIAL_VALUES)
        .fold(0, |acc, (population, value)| {
            acc + *population as i8 * value
        })
        - piece_populations[BLACK]
            .iter()
            .zip(&PIECE_MATERIAL_VALUES)
            .fold(0, |acc, (population, value)| {
                acc + *population as i8 * value
            })
}

#[derive(Copy, Clone)]
struct PieceColorOffsets([[isize; 5]; 2]);
type PieceColorPair = (Piece, Color);

impl Index<PieceColorPair> for PieceColorOffsets {
    type Output = isize;

    fn index(&self, (piece, color): PieceColorPair) -> &isize {
        &self.0[color as usize][piece as usize]
    }
}

impl Index<Color> for PieceColorOffsets {
    type Output = [isize; 5];

    fn index(&self, color: Color) -> &[isize; 5] {
        &self.0[color as usize]
    }
}

impl IndexMut<PieceColorPair> for PieceColorOffsets {
    fn index_mut(&mut self, (piece, color): PieceColorPair) -> &mut isize {
        &mut self.0[color as usize][piece as usize]
    }
}

impl Index<PieceColorPair> for MaterialEvaluator {
    type Output = isize;

    fn index(&self, (piece, color): PieceColorPair) -> &isize {
        &self.populations[(piece, color)]
    }
}

impl IndexMut<PieceColorPair> for MaterialEvaluator {
    fn index_mut(&mut self, (piece, color): PieceColorPair) -> &mut isize {
        &mut self.populations[(piece, color)]
    }
}

impl<'a> AuxiliaryStruct<'a> for MaterialEvaluator {
    type Source = &'a Position;
    fn initialize(position: Self::Source) -> Self {
        let n_white_rook = position.get_number_of(Piece::ROOK, WHITE) as isize;
        let n_black_rook = position.get_number_of(Piece::ROOK, BLACK) as isize;
        let n_white_bishop = position.get_number_of(Piece::BISHOP, WHITE) as isize;
        let n_black_bishop = position.get_number_of(Piece::BISHOP, BLACK) as isize;
        let n_white_knight = position.get_number_of(Piece::KNIGHT, WHITE) as isize;
        let n_black_knight = position.get_number_of(Piece::KNIGHT, BLACK) as isize;
        let n_white_queen = position.get_number_of(Piece::QUEEN, WHITE) as isize;
        let n_black_queen = position.get_number_of(Piece::QUEEN, BLACK) as isize;
        let n_white_pawn = position.get_number_of(Piece::PAWN, WHITE) as isize;
        let n_black_pawn = position.get_number_of(Piece::PAWN, BLACK) as isize;

        MaterialEvaluator {
            table_index: 1838 // Constant offset to avoid negative indexes
                + INDEX_OFFSETS[(Piece::ROOK, WHITE)] * n_white_rook
                + INDEX_OFFSETS[(Piece::ROOK, BLACK)] * n_black_rook
                + INDEX_OFFSETS[(Piece::BISHOP, WHITE)] * n_white_bishop
                + INDEX_OFFSETS[(Piece::BISHOP, BLACK)] * n_black_bishop
                + INDEX_OFFSETS[(Piece::KNIGHT, WHITE)] * n_white_knight
                + INDEX_OFFSETS[(Piece::KNIGHT, BLACK)] * n_black_knight
                + INDEX_OFFSETS[(Piece::QUEEN, WHITE)] * n_white_queen
                + INDEX_OFFSETS[(Piece::QUEEN, BLACK)] * n_black_queen
                + INDEX_OFFSETS[(Piece::PAWN, WHITE)] * n_white_pawn
                + INDEX_OFFSETS[(Piece::PAWN, BLACK)] * n_black_pawn,

            // NOTE: White is index 1 and Black index 0, see utils/board_utils.rs
            // See also in utils/board_utils the piece indexes
            populations: PieceColorOffsets([
                [
                    n_black_knight,
                    n_black_bishop,
                    n_black_rook,
                    n_black_queen,
                    n_black_pawn,
                ],
                [
                    n_white_knight,
                    n_white_bishop,
                    n_white_rook,
                    n_white_queen,
                    n_white_pawn,
                ],
            ]),

            valid_index: n_white_rook <= 2
                && n_black_rook <= 2
                && n_white_bishop <= 2
                && n_black_bishop <= 2
                && n_white_knight <= 2
                && n_black_knight <= 2
                && n_white_queen <= 1
                && n_black_queen <= 1, // NOTE: There can't be more than 8 pawns per side
        }
    }
}

// TODO check that we are in a valid material constellation
impl MaterialEvaluator {
    // We update the material index on capture or promotion
    pub fn capture_piece(&mut self, captured_piece: Piece, color: Color) {
        self.populations[(captured_piece, color)] -= 1;
        self.table_index -= INDEX_OFFSETS[(captured_piece, color)];
    }

    pub fn uncapture_piece(&mut self, captured_piece: Piece, color: Color) {
        self.populations[(captured_piece, color)] += 1;
        self.table_index += INDEX_OFFSETS[(captured_piece, color)];
    }

    pub fn evaluation(&self, side_to_move: Color) -> f32 {
        let side_multiplier = side_multiplier(side_to_move);
        if self.valid_index {
            side_multiplier * f32::from(MATERIAL_TABLE[self.table_index as usize])
        } else {
            // If the index is not a common constellation, we compute everything by hand
            side_multiplier * f32::from(material_value(&self.populations))
        }
    }
}
