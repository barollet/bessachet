//! The color of a player

/*
 * The color is represented as a boolean that we use when
 * possible as an index in 2-sized arrays
 */

pub type Color = bool;
/// Black player color
pub const BLACK: Color = false;
/// White player color
pub const WHITE: Color = true;
