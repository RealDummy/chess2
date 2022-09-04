use crate::chess::bit_set::Set;

#[derive(Clone, Copy, PartialEq)]

pub enum Player {
    White = 0,
    Black = 1,
}
impl Player {
    pub fn index(&self) -> usize {
        *self as usize
    }
    pub fn invert(&self) -> Self {
        match self {
            Player::White => Player::Black,
            Player::Black => Player::White,
        }
    }
    pub fn iter() -> impl Iterator<Item = Player> {
        [Player::White, Player::Black].iter().copied()
    }
}
#[derive(Clone, Copy, PartialEq)]
pub enum Piece {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl Piece {
    pub fn index(&self) -> usize {
        *self as usize
    }
    pub fn get_char(&self, player: Player) -> char {
        PIECE_REPR[self.index()][player.index()]
    }
    pub fn iter() -> impl Iterator<Item = Piece> {
        use Piece::*;
        [Pawn,Knight,Bishop,Rook,Queen,King].iter().copied()
    }
}

const PIECE_REPR: [[char;2]; 6] = [
    ['♟','♙'],
    ['♞','♘'],
    ['♝','♗'],
    ['♜','♖'],
    ['♛','♕'],
    ['♚','♔'],
];

pub const WHITE_START: [Set; 6] = [
    0x00ff000000000000,
    0x4200000000000000,
    0x2400000000000000,
    0x8100000000000000,
    0x0800000000000000,
    0x1000000000000000,
];
pub const BLACK_START: [Set; 6] = [
    0xff00,
    0x0042,
    0x0024,
    0x0081,
    0x0008,
    0x0010,
];