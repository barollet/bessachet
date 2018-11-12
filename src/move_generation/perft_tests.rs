// This is a module for perft results testing
// All perft results and positions are taken from
// https://www.chessprogramming.org/Perft_Results

use board::Board;

use move_generation::init_magic_tables;

use utils::*;

use move_generation::*;

// Runs a perft test of the given depth on the given board
fn perft(board: &mut Board, depth: u8) -> usize {
    // TODO Bulk gen when legal moves are ready
    if depth == 0 {
        return 1;
    }
    let moves: Vec<_> = board.possible_moves().collect();
    let mut sum = 0;
    for mov in moves {
        board.make(mov);

        if !board.is_king_checked() {
            sum += perft(board, depth-1);
            if depth == 2 {
                println!("{}: {}", mov, perft(board, depth-1));
            }
        } else {
            println!("checked");
        }

        board.unmake(mov);
    }

    sum
}

#[test]
fn initial_position() {
    init_magic_tables();

    let mut board = Board::initial_position();

    //board.make(Move::quiet_move(Square::from_char_rank_file('g', '1'), Square::from_char_rank_file('h', '3')));
    //board.make(Move::quiet_move(Square::from_char_rank_file('b', '2'), Square::from_char_rank_file('b', '4')));
    //board.unmake(Move::quiet_move(Square::from_char_rank_file('b', '2'), Square::from_char_rank_file('b', '4')));

    //board.make(&Move::quiet_move(Square::from_char_rank_file('f', '1'), Square::from_char_rank_file('b', '5')));

    println!("{}", board);

    for mov in board.possible_moves() {
        println!("{} {}", mov.transpose(), mov);
    }

    //board.debug_move_counts();

    //assert_eq!(perft(&mut board, 2), 622);
    //assert_eq!(perft(&mut board, 1), 18);

    assert_eq!(perft(&mut board, 4), 197_281);
    //assert_eq!(perft(&mut board, 3), 8902);
    //
    //assert_eq!(perft(&mut board, 2), 400);
    
    
}
