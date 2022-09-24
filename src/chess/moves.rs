use crate::chess::piece::Piece;

#[derive(Debug, Clone)]
pub enum MoveType {
    Quiet,
    Capture(Piece),
    EnPassent,
    Castle(CastleType),
    Promotion(Piece),
    CaptureAndPromotion(Piece, Piece),
}

#[derive(Debug, Clone, Copy)]
pub struct MoveSquares {
    pub from: u8,
    pub to: u8,
    pub promote: Option<Piece>,
}
#[derive(Debug, Clone)]
pub struct Move {
    pub from: u8,
    pub to: u8,
    pub move_type: MoveType
}

#[derive(Debug, Clone, Copy)]
pub enum CastleType {
    KingSide,
    QueenSide,
}