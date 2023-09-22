use crate::chess::piece::Piece;

#[derive(Debug, Clone)]
pub enum MoveType {
    PawnPush,
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

pub fn to_algerbraic(square: u8) -> String {
    let mut res = String::with_capacity(2);
    let file = match square % 8 {
        0 => 'a',
        1 => 'b',
        2 => 'c',
        3 => 'd',
        4 => 'e',
        5 => 'f',
        6 => 'g',
        7 => 'h',
        _ => '?',
    };
    let rank = match square/8 {
        0 => '8',
        1 => '7',
        2 => '6',
        3 => '5',
        4 => '4',
        5 => '3',
        6 => '2',
        7 => '1',
        _ => '?',
    };
    res.push(file);
    res.push(rank);

    return res;
}