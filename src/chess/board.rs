use std::convert::TryInto;
use std::slice::Iter;
use std::vec::Vec;

use super::input::{self, get_square};
use crate::chess::piece::{Piece, BLACK_START, WHITE_START, Player, SpecefiedPiece};
use crate::chess::bit_set;
pub use super::moves::{Move, MoveType, MoveSquares, CastleType};
use super::generator::{PossibleMoveGenerator, SliderMasks};
use bit_set::Set;
use termion::{color};
use log::{info};

pub fn file<N: Into<usize> + Copy + std::ops::Div + TryFrom<usize>>(square: N) -> N {
    (square.into() % 8).try_into().ok().unwrap()
}
pub fn rank<N: Into<usize> + Copy + std::ops::Div + TryFrom<usize>>(square: N) -> N {
    (square.into() / 8).try_into().ok().unwrap()
}   

fn pretty_file(file: u8) -> char {
    let a: u8 = 'a' as u8;
    (a + file).into()
}
fn pretty_rank(rank: u8) -> char {
    let eight: u8 = '8' as u8;
    (eight - rank).into()

}
fn pretty_square(square: u8) -> String {
    format!("{}{}", pretty_file(file(square)), pretty_rank(rank(square)))
}   



pub fn adjust_square<N: TryInto<i32> + Copy + std::ops::Add + TryFrom<i32>>(square: N, file: i32, rank: i32) -> N {
   (square.try_into().ok().unwrap() + file + rank * 8).try_into().ok().unwrap()
}

pub fn adjust_square_checked<N: TryInto<i32> + Into<usize> + TryFrom<usize> + std::ops::Div + Copy + std::ops::Add + TryFrom<i32>>(square:N, adj_file:i32, adj_rank: i32) -> Option<N> {
    let f = file(square).try_into().ok()?;
    let r = rank(square).try_into().ok()?;
    let nf = adj_file + f;
    let nr = adj_rank + r;
    if nf >= 8 || nf < 0 || nr >= 8 || nr < 0 {
        return None;
    }
    let new_square: i32 = nf + nr * 8;
    match new_square {
        0..=63 => new_square.try_into().ok(),
        _ => None,
    }
}

#[derive(Clone, Copy)]

enum CastleRights {
    None = 0b0,
    KingSide = 0b1,
    QueenSide = 0b10,
    Both = 0b11,
}
impl CastleRights {
    fn iter<'a>(&'a self) -> Iter<CastleType> {
        match self {
            Self::None => [].iter().to_owned(),
            Self::KingSide => [CastleType::KingSide].iter().to_owned(),
            Self::QueenSide => [CastleType::QueenSide].iter().to_owned(),
            Self::Both => [CastleType::KingSide, CastleType::QueenSide].iter().to_owned(),
        }
    }
}

impl From<u8> for CastleRights {
    fn from(a: u8) -> Self {
        match a & 0b11 {
            0 => Self::None,
            0b1 => Self::KingSide,
            0b10 => Self::QueenSide,
            _ => Self::Both,
        }
    }
}

impl CastleRights {
   pub fn invert(&self) -> Self {
        ((!(*self as u8)) & 0b11).into()
   }
}
#[derive(Clone)]
struct LostCastleRights(CastleRights);

impl LostCastleRights {
    pub fn get_rights(&self) -> CastleRights {
        self.0.invert()
    }
    pub fn lose(&mut self, right: CastleRights) {
        self.0 = (self.0 as u8 | right as u8).into()
    }
}

//things to add: king pos
#[derive(Clone)]
struct MoveGenCache {
    pub friends: Set,
    pub enemies: Set,
    pub all_pieces: Set,
    pub pinned: Set,
    pub pinners: Set,
    pub check: Set,
}

#[derive(Clone)]
pub struct Board {
    active: Player,
    pieces: [[Set;6]; 2],
    mailbox: Mailbox,
    half_move: u32,
    en_passant_target: Option<u8>,
    last_irreversable: u32,
    cache: Option<MoveGenCache>,
    lost_castle_rights: [LostCastleRights; 2],


}
#[derive(Clone, Copy, Debug)]
struct MailboxPiece {
    pub player: Player,
    pub piece: Piece,
}


#[derive(Clone)]
struct Mailbox {
    squares: [Option<MailboxPiece>; 65] //not a typo, 0 has 64 trailing 0's, so squares[64] should be None always!
}

impl Board {
    pub fn new() -> Self{
        let pieces = [WHITE_START.clone(), BLACK_START.clone()];
        Board {
            pieces,
            active: Player::White,
            mailbox: Mailbox::from(pieces),
            half_move: 0,
            en_passant_target: None,
            last_irreversable: 0,
            cache: None,
            lost_castle_rights: [LostCastleRights(CastleRights::None), LostCastleRights(CastleRights::None)],
        }
    }
    pub fn empty() -> Self {
        let pieces = [[0;6];2];
        Board {
            pieces,
            active: Player::White,
            mailbox: Mailbox::from(pieces),
            half_move: 0,
            en_passant_target: None,
            last_irreversable: 0,
            cache: None,
            lost_castle_rights: [LostCastleRights(CastleRights::None), LostCastleRights(CastleRights::None)],
        }
    }
    pub fn from_fen(fen: &str) -> Self {
        let mut board = Self::empty();
        let mut rank: i32 = 0;
        let mut file: i32 = 0;
        let mut chiter = fen.chars();

        //set up board
        chiter.any(|c| {
            match c {
                '1'..='8' => {file += c.to_digit(10).unwrap() as i32 - 1;},
                'k' => bit_set::set(board.get_set_mut(Player::Black, Piece::King), adjust_square(0u8, file, rank)),
                'K' => bit_set::set(board.get_set_mut(Player::White, Piece::King), adjust_square(0u8, file, rank)),
                'q' => bit_set::set(board.get_set_mut(Player::Black, Piece::Queen), adjust_square(0u8, file, rank)),
                'Q' => bit_set::set(board.get_set_mut(Player::White, Piece::Queen), adjust_square(0u8, file, rank)),
                'r' => bit_set::set(board.get_set_mut(Player::Black, Piece::Rook), adjust_square(0u8, file, rank)),
                'R' => bit_set::set(board.get_set_mut(Player::White, Piece::Rook), adjust_square(0u8, file, rank)),
                'b' => bit_set::set(board.get_set_mut(Player::Black, Piece::Bishop), adjust_square(0u8, file, rank)),
                'B' => bit_set::set(board.get_set_mut(Player::White, Piece::Bishop), adjust_square(0u8, file, rank)),
                'n' => bit_set::set(board.get_set_mut(Player::Black, Piece::Knight), adjust_square(0u8, file, rank)),
                'N' => bit_set::set(board.get_set_mut(Player::White, Piece::Knight), adjust_square(0u8, file, rank)),
                'p' => bit_set::set(board.get_set_mut(Player::Black, Piece::Pawn), adjust_square(0u8, file, rank)),
                'P' => bit_set::set(board.get_set_mut(Player::White, Piece::Pawn), adjust_square(0u8, file, rank)),
                '/' => {return false;},
                ' ' => {return true;},
                _ => {return false},
            }
            file += 1;
            if file > 7 {
                file = 0;
                rank += 1;
            }
            false
        });
        //get active player
        let mut active = Player::White;
        chiter.any(|c| {
            if c == ' ' {
                return true;
            }
            active = match c {
                'b' => Player::Black,
                'w' | _ => Player::White,
            };
            false

        });
        //castling rights
        let mut black_castle_rights = LostCastleRights(CastleRights::None);
        let mut white_castle_rights = LostCastleRights(CastleRights::None);
        chiter.any(|c| {
            if c == ' ' {
                return true;
            }
            match c {
                'q' => {black_castle_rights.lose(CastleRights::QueenSide)}
                'k' => {black_castle_rights.lose(CastleRights::KingSide)},
                'Q' => {white_castle_rights.lose(CastleRights::QueenSide)},
                'K' => {white_castle_rights.lose(CastleRights::KingSide)},
                _ => (),
            }
            false
        });
        black_castle_rights = LostCastleRights(black_castle_rights.get_rights());
        white_castle_rights = LostCastleRights(white_castle_rights.get_rights());

        let ept = match get_square(&chiter.as_str()[0..2]) {
            Ok(s) => Some(s),
            Err(e) => {None}
        };

        chiter.any(|c| c == ' ');
        
        let last_pawn_move: String = chiter.by_ref().take_while(|c| c.is_ascii_digit()).collect();
        let last_irreversable: u32 = last_pawn_move.parse().ok().unwrap_or(0);

        let move_count: String = chiter.by_ref().take_while(|d| d.is_ascii_digit()).collect();
        let half_move: u32 = move_count.parse().ok().unwrap_or(0) * 2 + active.index() as u32;
        Self {
            active,
            pieces: board.pieces,
            en_passant_target: ept,
            mailbox: Mailbox::from(board.pieces),
            half_move,
            last_irreversable,
            cache: None,
            lost_castle_rights: [white_castle_rights, black_castle_rights]
        }

    }

    fn get_set(&self, player: Player, piece: Piece) -> Set {
        self.pieces[player.index()][piece.index()]
    }
    fn get_set_mut<'a>(&'a mut self, player: Player, piece: Piece) -> &'a mut Set{
        &mut self.pieces[player.index()][piece.index()]
    }
    
    fn fmv(&self, from: u8, to: u8, promote: Option<Piece>) -> String {
        let res = match promote {
            None => format!("{}{}({})",pretty_square(from), pretty_square(to), self.half_move),
            Some(piece) => format!("{}{}{}({})", pretty_square(from), pretty_square(to), piece.get_char(Player::White), self.half_move)
        };
        res
        
    }

    fn draw_board(buffer: &[char; 64], player: Player) {
        println!("{}  ┏━━━━━━━━━━━━━━━━━━━━━━━━┓", color::Fg(color::White));
        let square_iter  = match player {
            Player::White => [0,1,2,3,4,5,6,7],
            Player::Black => [7,6,5,4,3,2,1,0],
        }.into_iter();
        for i in square_iter.clone() {
            let rank = match i {
                0 => '8',
                1 => '7',
                2 => '6',
                3 => '5',
                4 => '4',
                5 => '3',
                6 => '2',
                7 => '1',
                _ => ' ',
            };
            print!("{} ┃", rank);
            for j in square_iter.clone() {
                let is_black = (i%2)^(j%2) == 1;
                let spot = buffer[(i*8 + j) as usize];
                if is_black {
                    print!("{}{} {} ", color::Bg(color::Black),color::Fg(color::White), spot)
                }
                else {
                    print!("{}{} {} ", color::Bg(color::LightBlack),color::Fg(color::White), spot)
                }
            }
            println!("{}┃",color::Bg(color::Reset));
        }
        println!("  ┗━━━━━━━━━━━━━━━━━━━━━━━━┛");
        if player == Player::White {
            println!("    A  B  C  D  E  F  G  H ");
        }
        else {
            println!("    H  G  F  E  D  C  B  A ")
        }
    }
    pub fn show(&self) {
        let mut buffer = [' '; 64];
        for p in self.mailbox.iter() {  
            buffer[p.square as usize] = p.piece.get_char(p.player);
        }
        Board::draw_board(&buffer, self.active);
    }
    pub fn force_move(&mut self, m: MoveSquares, gen: &PossibleMoveGenerator) -> bool {
        let p = match self.mailbox.get(m.from) {
            Some(n) => n,
            None => {return false},
        };
        let moves = bit_set::union(gen.get_moves(&p), gen.get_attacks(&p));
        if bit_set::get(moves, m.to) == 0 {
            return false;
        }
        let set = self.get_set_mut(p.player, p.piece);
        bit_set::unset(set, m.from);
        bit_set::set(set, m.to);
        match self.mailbox.force_move(&m) {
            Some(p) => {
                bit_set::unset(self.get_set_mut(self.active.invert(), p.piece), p.square)
            },
            None => ()
        }
        true
    }
    fn get_square_attackers(&self, square: u8, gen: &PossibleMoveGenerator) -> Set {
        Piece::iter().fold(0, |a,piece| {
            let attacks = gen.get_attacks(&SpecefiedPiece { player: self.active, piece, square });
            let attackers = bit_set::intersect(self.get_set(self.active.invert(), piece), attacks);
            bit_set::union(a, attackers)
        })
    }
    fn has_unblocked_square_attackers(&self, square: u8, gen: &PossibleMoveGenerator, slide: &SliderMasks, all_pieces: Set) -> bool {
        let attackers = self.get_square_attackers(square, gen);
        bit_set::iter_pos(attackers).any(|attacker|{
            bit_set::intersect(slide.get(attacker, square), all_pieces) == 0
        })
    } 
    fn create_cache(&mut self, gen: &PossibleMoveGenerator, slide: &SliderMasks) {
        let opponent = self.active.invert(); 
        ////info!("current player: {:?}", self.active);
        let friends = self.pieces[self.active.index()].iter()
            .fold(0,|a,&x| bit_set::union(a, x));
        let enemies = self.pieces[opponent.index()].iter()
            .fold(0,|a,&x| bit_set::union(a, x));
        let all_pieces = bit_set::union(friends, enemies);
        let king_pos = bit_set::lsb_pos(self.get_set(self.active, Piece::King));
        let king_attackers:Set = self.get_square_attackers(king_pos, gen);

        let mut pinned:Set = 0;
        let mut pinners:Set = 0;
        let mut check:Set = 0;

        for attacker in bit_set::iter_pos(king_attackers) {
            let attack =  slide.get(attacker, king_pos);
            let blockers = bit_set::intersect(attack, bit_set::difference(king_attackers, all_pieces));
            let count = bit_set::count(blockers);
            if count == 0 {
                bit_set::set(&mut check, attacker);
            }
            let pf = bit_set::intersect(attack, friends);
            if bit_set::count(pf) == 1 && count == 1 {
                pinned = bit_set::union(pinned, pf);
                bit_set::set(&mut pinners, attacker);
            }
        }
        self.cache = Some(MoveGenCache {
            friends,
            enemies,
            all_pieces,
            pinned,
            pinners,
            check
        });
    }

    pub fn check_move(&mut self, gen: &PossibleMoveGenerator, slide: &SliderMasks, candidate: MoveSquares) -> bool {
        if let None = self.cache {
            self.create_cache(gen, slide);
        }
        self.check_move_unchecked(gen, slide, candidate)
    }
    fn king_pos(&self, player: Player) -> u8 {
        bit_set::lsb_pos(self.get_set(player, Piece::King))
    }

    fn check_en_passant(&self, gen: &PossibleMoveGenerator, slide: &SliderMasks, candidate: MoveSquares) -> bool {
        let enemy_pawn = adjust_square(candidate.to, 0, match self.active {
            Player::White => 1,
            Player::Black => -1,
        });
        if let Some(_) = self.mailbox.get(candidate.to) {
            return false;
        }
        let king_pos = self.king_pos(self.active);
        //check for checks
        let checks = self.cache.as_ref().unwrap().check;
        let target_check = bit_set::intersect(checks, 1u64 << enemy_pawn);

        if bit_set::intersect(1 << candidate.from, self.cache.as_ref().unwrap().pinned) != 0 {
            return false;
        }

        for set in bit_set::iter(bit_set::difference(target_check, checks)) {
            if bit_set::intersect(slide.get(king_pos, bit_set::lsb_pos(set)), set) == 0 {
                return false;
            }
        }
        //check for pin
        if rank(enemy_pawn) == rank(king_pos) {

            let rnq = [Piece::Rook, Piece::Queen].iter().fold(0, |a,&p| {
                bit_set::union(a, self.get_set(self.active.invert(), p))
            });
            let rank_mask = 0xFFu64 << 8 * rank(enemy_pawn);
            let problems = bit_set::intersect(rank_mask, rnq);
            let blockers = [enemy_pawn, candidate.from].iter().fold(self.cache.as_ref().unwrap().all_pieces, |a, &x| {
                bit_set::difference(1u64 << x, a)
            });
            let pinned = bit_set::iter_pos(problems).any(|x| {
                bit_set::intersect(blockers, slide.get(king_pos, x)) == 0
            });
            if pinned {
                return false;
            }
        }
        true
    }

    fn check_castle(&mut self, gen: &PossibleMoveGenerator, slide: &SliderMasks, side: &CastleType) -> bool {
        let check = self.cache.as_ref().unwrap().check;
        if check != 0 {
            return false;
        }
        let rook_pos = match self.active {
            Player::White => {
                match side {
                    CastleType::KingSide => 63,
                    CastleType::QueenSide => 56,
                }
            },
            Player::Black => {
                match side {
                    CastleType::KingSide => 7,
                    CastleType::QueenSide => 0,
                }
            }
        };
        if let Some( sp ) = self.mailbox.get(rook_pos) {
            if sp.piece != Piece::Rook || sp.player != self.active {
                self.lost_castle_rights[self.active.index()].lose(match side {
                    CastleType::KingSide => CastleRights::KingSide,
                    CastleType::QueenSide => CastleRights::QueenSide,
                });
                return false;
            }
        }
        let king_pos = self.king_pos(self.active);
        let between = slide.get(king_pos, rook_pos);
        let all_pieces = self.cache.as_ref().unwrap().all_pieces;
        if bit_set::intersect(all_pieces, between) != 0 {
            return false;
        }
        let castle_square = Self::get_castle_square(self.active, side);
        let king_slide = slide.get(king_pos, castle_square);
        
        bit_set::iter_pos(bit_set::union(king_slide, 1 << castle_square)).all(|p| {
            !self.has_unblocked_square_attackers(p, gen, slide, all_pieces)
        })
        
    }

    fn check_move_unchecked(&self, gen: &PossibleMoveGenerator, slide: &SliderMasks, candidate: MoveSquares) -> bool {
        let MoveGenCache{
            friends,
            all_pieces,
            pinned,
            pinners,
            check,
            ..
        } = self.cache.as_ref().unwrap().clone();
        let square = candidate.from;
        let ss = 1u64 << square;
        let new_square = candidate.to;
        let nss = 1u64 << new_square;
        let piece = match self.mailbox.get(square) {
            Some(n) => n,
            None => {
                //info!("move {} rejected: no piece there", self.fmv(square, new_square, None));
                return false;
            }
        };
        if bit_set::intersect(friends, nss) != 0 {
            //info!("move {} rejected: lands on friend", self.fmv(square, new_square, None));
            return false;
        }
        let s = slide.get(piece.square, new_square);
        let slide_blockers = bit_set::intersect(s, all_pieces);
        if slide_blockers != 0 {
            //info!("move {} rejected: slide blocked", self.fmv(square, new_square, None));
            return false;
        }
        let king_pos = bit_set::lsb_pos(self.get_set(self.active, Piece::King));
        if bit_set::intersect(ss, pinned) != 0 { //if piece is pinned
            if bit_set::iter_pos(pinners).any(|pinner|{ //for each pinner, check if any
                bit_set::intersect( //new move doesnt intersect with pinner attack
                    bit_set::union(
                        1u64 << pinner, 
                        slide.get(pinner, king_pos)
                    ),
                    nss
                ) == 0 &&
                bit_set::intersect( // and old position does intersect attack
                    slide.get(pinner, king_pos), 
                    ss
                ) != 0
            }) {
                //info!("move {} rejected: piece is pinned", self.fmv(square, new_square, None));
                return false;
            }
        }
        match piece.piece {
            Piece::King => {
                let new_attackers = self.get_square_attackers(new_square, gen);
                let is_attacked = bit_set::iter_pos(new_attackers).any(|attacker_pos|{
                    let attack = slide.get(new_square, attacker_pos);
                    let blockers = bit_set::intersect(bit_set::difference(1 << piece.square, all_pieces), attack);
                    bit_set::count(blockers) == 0
                });
                if is_attacked {
                    //info!("move {} rejected: king moved into check", self.fmv(piece.square, new_square, None));
                    return false;
                }
            }
            _ => {
                if bit_set::count(check) != 0 {
                    let resolved_checks = bit_set::iter_pos(check).all(|check_pos| {
                        let attack = slide.get(king_pos, check_pos);
                        let block = bit_set::intersect(attack, nss);
                        let capture = bit_set::intersect(nss, 1u64 << check_pos);
                        capture != 0 || block != 0
                    });
                    if !resolved_checks {
                       //info!("move {} rejected: king still in check", self.fmv(piece.square, new_square, None));
                        return false;
                    }
                }
            }
        }
        true
    }

    fn get_castle_square(player: Player, castle_type: &CastleType ) -> u8 {
        match player {
            Player::White => {
                match castle_type {
                    CastleType::KingSide => 62,
                    CastleType::QueenSide => 58,
                }
            },
            Player::Black => {
                match castle_type {
                    CastleType::KingSide => 6,
                    CastleType::QueenSide => 2,
                }
            }
        }
    }

    fn get_castle_rook(player: Player, castle_type: &CastleType) -> u8 {
        match player {
            Player::White => {
                match castle_type {
                    CastleType::KingSide => 63,
                    CastleType::QueenSide => 56,
                }
            },
            Player::Black => {
                match castle_type {
                    CastleType::KingSide => 7,
                    CastleType::QueenSide => 0,
                }
            }
        }
    }
    fn generate_promotions(sp: &SpecefiedPiece) -> Iter<Option<Piece>> {
        match sp.piece {
            Piece::Pawn => {
               match (match sp.player {
                    Player::White => 1,
                    Player::Black => 6,
                } == rank(sp.square) as i32) {
                    true => [Some(Piece::Queen), Some(Piece::Rook), Some(Piece::Knight), Some(Piece::Bishop)].iter(),
                    false => [None].iter(),
                }
            },
            _ => [None].iter(),
        }
    }
    pub fn generate_legal(&mut self, gen: &PossibleMoveGenerator, slide: &SliderMasks) -> Vec<Move> {
        ////info!("---starting generation for half move {}----", self.half_move);
        let mut res: Vec<Move> =vec![];
        self.create_cache(gen, slide);
        let enemies = self.cache.as_ref().unwrap().enemies;
        let friends = self.cache.as_ref().unwrap().friends;
        for piece in self.mailbox.iter().filter(|p| p.player == self.active) {

            let possible_moves = bit_set::difference(friends, gen.get_moves(&piece));
            let quiet_moves = bit_set::difference(enemies, possible_moves);

            let possible_captures = gen.get_attacks(&piece);
            let captures = bit_set::intersect(enemies, possible_captures);

            for &promote in Self::generate_promotions(&piece){
                for square in bit_set::iter_pos(quiet_moves) {
                    let candidate = MoveSquares {
                        from: piece.square,
                        to: square,
                        promote,
                    };
                    
                    if self.check_move_unchecked(gen, slide, candidate) {
                        res.push(Move {
                            from: piece.square,
                            to: square,
                            move_type: match promote {
                                None => MoveType::Quiet,
                                Some(p) => MoveType::Promotion(p),
                            },
                        })
                    }
                }
                
                for square in bit_set::iter_pos(captures) {
                    let candidate = MoveSquares {
                        from: piece.square,
                        to: square,
                        promote,
                    };
                    if self.check_move_unchecked(gen, slide, candidate) {
                        res.push(Move {
                            from: piece.square,
                            to: square,
                            move_type: match promote {
                                None => MoveType::Capture(self.mailbox.get(square).unwrap().piece),
                                Some(p) => MoveType::CaptureAndPromotion(self.mailbox.get(square).unwrap().piece, p)
                            }
                        })
                    }
                }
            }
        }
        if let Some(ept) = self.en_passant_target {
            let possible_en_passants = gen.get_attacks(&SpecefiedPiece{
                player: self.active.invert(),
                square: ept,
                piece: Piece::Pawn,
            });
            let en_passant_pawns = bit_set::intersect(possible_en_passants, self.get_set(self.active, Piece::Pawn));
            for square in bit_set::iter_pos(en_passant_pawns) {

                for &promote in Self::generate_promotions(&SpecefiedPiece{
                    player: self.active,
                    piece: Piece::Pawn,
                    square,

                }) {
                    if self.check_en_passant(gen, slide, MoveSquares {
                        to: ept,
                        from: square,
                        promote,
                    }) {
                        res.push(Move {
                            to: ept,
                            from: square,
                            move_type: MoveType::EnPassent,
                        })
                    }
                }
                
            }
        }
        self.lost_castle_rights[self.active.index()].get_rights().iter().for_each(|side| {
            if self.check_castle(gen, slide, side) {
                res.push(Move {
                    from: self.king_pos(self.active),
                    to: Self::get_castle_square(self.active, side),
                    move_type: MoveType::Castle(*side)
                })
            }
        });
        res
    }

    //DOES NOT CHECK IF MOVE IS LEGAL, panics if an empty square is moved.
    pub fn make_move(&mut self, m: &Move) {
        let piece = match self.mailbox.get(m.from) {
            Some(n) => n,
            None => {panic!("move {:?} attempted, but no piece is on {}", m, m.from);}
        };
        let set = self.get_set_mut(piece.player, piece.piece);
        bit_set::unset(set, m.from);
        bit_set::set(set, m.to);
        match m.move_type {
            MoveType::Capture(piece) => {
                if let Piece::King = piece {
                    self.show();
                    println!("{:?}", m);
                    panic!("King can be captured!");
                }
                match self.mailbox.force_move(&MoveSquares { from: m.from, to: m.to, promote: None }) {
                    Some(cap) => {
                        let set = self.get_set_mut(cap.player, cap.piece);
                        bit_set::unset(set, m.to);
                        true
                    },
                    None => panic!("move failed, no piece to capture"),
                };
                self.last_irreversable = self.half_move;
            },
            MoveType::EnPassent => {
                self.mailbox.force_move(&MoveSquares { from: m.from, to: m.to, promote: None });
                let square = adjust_square(m.to, 0, -self.active.pawn_dir(1));
                self.mailbox.set(square, None, self.active.invert());
                bit_set::unset(self.get_set_mut(self.active.invert(), Piece::Pawn), square);
            }
            MoveType::Quiet => {
                self.mailbox.force_move(&MoveSquares { from: m.from, to: m.to, promote: None });
            }
            MoveType::Castle(side) => {
                let rook_square = Self::get_castle_rook(self.active, &side);
                self.mailbox.force_move(&MoveSquares{from: m.from, to: m.to, promote: None});
                let to = (m.from + m.to) / 2;
                self.mailbox.force_move(&MoveSquares{
                    from: rook_square,
                    to,
                    promote: None,
                });
                bit_set::unset(self.get_set_mut(self.active, Piece::Rook), rook_square);
                bit_set::set(self.get_set_mut(self.active, Piece::Rook), to);
            },
            MoveType::Promotion(promote) => {
                bit_set::unset(self.get_set_mut(self.active, Piece::Pawn), m.to);
                bit_set::set(self.get_set_mut(self.active, promote), m.to);
                self.mailbox.force_move(&MoveSquares { from: m.from, to: m.to, promote: Some(promote) });
            }
            MoveType::CaptureAndPromotion(piece, promote) => {
                bit_set::unset(self.get_set_mut(self.active, Piece::Pawn), m.to);
                bit_set::set(self.get_set_mut(self.active, promote), m.to);
                match self.mailbox.force_move(&MoveSquares { from: m.from, to: m.to, promote: Some(promote) }) {
                    Some(cap) => {
                        let set = self.get_set_mut(cap.player, cap.piece);
                        bit_set::unset(set, m.to);
                        true
                    },
                    None => panic!("move failed, no piece to capture"),
                };
                self.last_irreversable = self.half_move;
            }
            _ => {
                panic!("not implimented yet")
            }
        }
            
        self.en_passant_target = None;
        match piece.piece{
            Piece::Pawn => {
                self.last_irreversable = self.half_move;
                if (rank(m.to) as i32 - rank(m.from) as i32).abs() == 2 {
                    self.en_passant_target = Some( (m.to + m.from) / 2 ); //really stupid to do it like this
                }
            }
            Piece::King => {
                self.lost_castle_rights[self.active.index()].lose(CastleRights::Both)
            },
            Piece::Rook => {
                if m.from == Self::get_castle_rook(self.active, &CastleType::KingSide) {
                    self.lost_castle_rights[self.active.index()].lose(CastleRights::KingSide);
                }
                else if m.from == Self::get_castle_rook(self.active, &CastleType::QueenSide) {
                    self.lost_castle_rights[self.active.index()].lose(CastleRights::QueenSide);
                }
            }
            _ => (),
        }
        self.half_move += 1;
        self.active = self.active.invert();
    }

    pub fn shitty_play(&mut self) {
        let gen = PossibleMoveGenerator::new();
        let slide = SliderMasks::new();
        loop {
            let psudo_moves = self.generate_legal(&gen, &slide);
            if psudo_moves.len() == 0 {
                println!("Checkmate or draw!");
            }
            self.show();
            loop {
                let s = input::get_input();
                let m = match input::read_uci(&s) {
                    Err(msg) => {
                        eprintln!("{}", msg);
                        continue;
                    }
                    Ok(m) => m,
                };
                match psudo_moves.iter().find(|&target| {
                    target.to == m.to && target.from == m.from
                } ) {
                    Some(m) => {
                        self.make_move(&m)
                    }
                    None => {
                        bit_set::show(self.cache.as_ref().unwrap().pinned);
                        bit_set::show(self.cache.as_ref().unwrap().pinners);
                        println!("Invalid Move (not in psuedolegal moves)"); 
                        continue;
                    }
                }
                break;
            }
            
        }
    }

    fn perft_impl(&mut self, n:u32, gen: &PossibleMoveGenerator, slide: &SliderMasks)-> usize {
        if n == 0 {
            return 1;
        }
        let mut res = 0;
        let moves = self.generate_legal(gen, slide);
        for m in &moves {
            let mut b = self.clone();
            b.make_move(m);
            res += b.perft_impl(n - 1, gen, slide);
        };
        res
    }

    pub fn perft(&mut self, n:u32, gen: &PossibleMoveGenerator, slide: &SliderMasks) -> usize {
        if n == 0 {
            return 1;
        }
        let mut res = 0;
        let moves = self.generate_legal(gen, slide);
        for m in &moves {
            let mut b = self.clone();
            b.make_move(m);
            let res2 = b.perft_impl(n - 1, gen, slide);
            println!("{}{}: {}", pretty_square(m.from), pretty_square(m.to), res2);
            res += res2;
        };
        res
    }
}
impl Mailbox {
    pub fn new() -> Self {
        Self {
            squares: [None; 65]
        }
    }

    pub fn iter(&self) -> MailboxIter<'_> {
        MailboxIter { curr: 0, arr: &self.squares }
    }

    pub fn from(pieces: [[Set; 6]; 2]) -> Self {
        let mut mailbox = Self::new();
        for player in Player::iter() {
            for piece in Piece::iter() {
                let set = pieces[player.index()][piece.index()];
                for i in 0..64usize {
                    if bit_set::get(set,i) != 0{
                        mailbox.squares[i] = Some(MailboxPiece{player, piece});
                    }
                }
            }
        }
        mailbox
    }

    pub fn get(&self, square: u8) -> Option<SpecefiedPiece> {
        let MailboxPiece {piece, player} = self.squares[square as usize]?;
        Some(SpecefiedPiece {piece, player, square})
    }
    pub fn set(&mut self, square: u8, piece: Option<Piece>, player: Player) {
        self.squares[square as usize] = (|| {
            let piece = piece?;
            Some(MailboxPiece {
                piece,
                player,
            })
        })()
    }
    pub fn force_move(&mut self, m: &MoveSquares) -> Option<SpecefiedPiece> {
        let t = self.get(m.from)?;
        self.squares[m.from as usize] = None;
        let res = self.get(m.to);
        self.squares[m.to as usize] = Some(MailboxPiece {piece: m.promote.unwrap_or(t.piece), player: t.player});
        res   
    }

}

pub struct MailboxIter<'a> {
    curr: u8,
    arr: &'a [Option<MailboxPiece>; 65],
}

impl<'a> Iterator for MailboxIter<'a> {
    type Item = SpecefiedPiece;

    fn next(&mut self) -> Option<Self::Item> {
        self.arr[self.curr.into()..].iter().zip(0..64u8)
            .filter_map(|(&o, i)| {
                match o {
                    Some(MailboxPiece { player, piece }) => {
                        let res = Some(SpecefiedPiece{player, piece, square: self.curr + i});
                        self.curr += i + 1;
                        res
                    },
                    None => None
                }
        }).next()
    }
}

#[cfg(test)]
mod test {
    use crate::chess::board::{Board, adjust_square};

    #[test]
    fn file_and_rank() {
        assert!( super::file(32u8) == 0);
        assert!( super::rank(32u8) == 4);
        assert!(adjust_square(28, 1, 1) == 37);
    }

    #[test]
    fn get_square_attackers() {
        let gen = super::PossibleMoveGenerator::new();
        let b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ");
        let square = super::adjust_square(0, 2, 3);
        let attackers = b.get_square_attackers(square, &gen);
        println!("{} {attackers}", super::pretty_square(square));
        assert!(attackers == 0x80080000)
    }

}