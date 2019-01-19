pub mod pawn_hash_table;

lazy_static! {
    pub static ref transposition_table: u64 = 5;
    pub static ref pawn_table: u64 = 5;
    pub static ref evaluation_table: u64 = 5;
}
