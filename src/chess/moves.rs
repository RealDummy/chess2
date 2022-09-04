use crate::chess::piece::Piece;

pub enum MoveType {
    Quiet,
    Check,
    Capture(Piece),
    EnPassent,
    Castle,
    Promotion(Piece),
}

pub struct MoveSquares {
    pub from: u8,
    pub to: u8,
    pub promote: Option<Piece>,
}

pub struct Move {
    from: u8,
    to: u8,
    move_type: MoveType
}

