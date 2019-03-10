use board::*;
use types::*;

#[test]
fn material_value() {
    println!("coucou");
    let mut board = Board::initial_position();
    println!("{}", board.material_evaluator.table_index);
    assert_eq!(board.material_evaluator.evaluation(), 0.0);

    board.material_evaluator.capture_piece(ROOK, WHITE);
    println!("{}", board.material_evaluator.table_index);
    assert_eq!(board.material_evaluator.evaluation(), -5.0);
}
