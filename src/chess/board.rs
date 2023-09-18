use std::convert::TryInto;
use std::hash::Hash;
use std::slice::Iter;
use std::vec::Vec;

use super::input::get_square;
use crate::chess::piece::{Piece, BLACK_START, WHITE_START, Player, SpecefiedPiece};
use crate::chess::bit_set;
use super::moves;
pub use super::moves::{Move, MoveType, MoveSquares, CastleType};
use super::generator::{PossibleMoveGenerator, SliderMasks};
use super::hash::{GameHasher};
use crate::chess::piece_square::eval_piece;
pub use super::hash::HashT;
use bit_set::Set;
use termion::{color};


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
    pub fn lose(&mut self, right: CastleRights) -> CastleRights {
        let lost_rights = !(self.0 as u8) & right as u8;
        self.0 = (self.0 as u8 | right as u8).into();
        lost_rights.into()
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
    half_move: u32,
    en_passant_target: Option<u8>,
    last_irreversable: u32,
    cache: Option<MoveGenCache>,
    lost_castle_rights: [LostCastleRights; 2],
    hash: HashT
}

impl Hash for Board {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(&self.hash.to_owned().to_ne_bytes());
    }
}

pub type EvalT = i16;

#[derive(Debug, Clone, Copy)]
pub enum Evaluation {
    MateWin(u16),
    MateLoss(u16),
    Draw,
    Value(EvalT),
}

impl Board {
    pub fn new(hasher: &GameHasher) -> Self{
        let pieces = [WHITE_START.clone(), BLACK_START.clone()];
        Self {
            pieces,
            active: Player::White,
            half_move: 1,
            en_passant_target: None,
            last_irreversable: 0,
            cache: None,
            lost_castle_rights: [LostCastleRights(CastleRights::None), LostCastleRights(CastleRights::None)],
            hash: 0
        }.update_hash(hasher)
    }
    pub fn empty(hasher: &GameHasher) -> Self {
        let pieces = [[0;6];2];
        Self {
            pieces,
            active: Player::White,
            half_move: 0,
            en_passant_target: None,
            last_irreversable: 0,
            cache: None,
            lost_castle_rights: [LostCastleRights(CastleRights::Both), LostCastleRights(CastleRights::Both)],
            hash: 0,
        }.update_hash(hasher)
    }
    pub fn from_fen(fen: &str, hasher: &GameHasher) -> Result<Self, &'static str> {
        let mut board = Self::empty(hasher);
        let mut rank: i32 = 0;
        let mut file: i32 = 0;
        let mut chiter = fen.chars();
        let mut error = false;
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
                _ => {error = true; return false},
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
                'w' => Player::White,
                _ => {
                    error = true;
                    Player::White
                }
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
                'q' => {black_castle_rights.lose(CastleRights::QueenSide);}
                'k' => {black_castle_rights.lose(CastleRights::KingSide);},
                'Q' => {white_castle_rights.lose(CastleRights::QueenSide);},
                'K' => {white_castle_rights.lose(CastleRights::KingSide);},
                _ => {
                    error = true
                },
            }
            false
        });
        black_castle_rights = LostCastleRights(black_castle_rights.get_rights());
        white_castle_rights = LostCastleRights(white_castle_rights.get_rights());


        let ept_str: String = chiter.by_ref().take_while(|&c| {
            c.is_ascii_alphanumeric() || c == '-'
        }).collect();

        let ept = match get_square(&ept_str) {
            Ok(s) => Some(s),
            Err(_) => None,
        };
        if ept.is_none() && ept_str != "-" {
            error = true;
        }
        //chiter.any(|c| c == ' ');
        
        let last_irreversable: String = chiter.by_ref().take_while(|c| c.is_ascii_digit()).collect();
        let last_irreversable: u32 = last_irreversable.parse().ok().unwrap_or_else(|| {error = true; 0});
        
        let move_count: String = chiter.by_ref().take_while(|d| d.is_ascii_digit()).collect();
        let half_move: u32 = move_count.parse().ok().unwrap_or_else(|| {error = true; 1}) * 2 + active.index() as u32;
        if error {
            return Err("invalid fen");
        }
        Ok(Self {
            active,
            pieces: board.pieces,
            en_passant_target: ept,
            half_move,
            last_irreversable,
            cache: None,
            lost_castle_rights: [white_castle_rights, black_castle_rights],
            hash: 0,
        }.update_hash(hasher))

    }

    fn get_set(&self, player: Player, piece: Piece) -> Set {
        self.pieces[player.index()][piece.index()]
    }
    fn get_set_mut<'a>(&'a mut self, player: Player, piece: Piece) -> &'a mut Set{
        &mut self.pieces[player.index()][piece.index()]
    }
    
    fn get_piece(&self, square: u8, player: Player) -> Option<Piece> {
        for piece in Piece::iter() {
            let set = self.get_set(player, piece);
            if bit_set::get(set, square) != 0 {
                return Some(piece);
            }
        }
        None
    }
    fn get_piece_any(&self, square: u8) -> Option<Piece> {
        let w = self.get_piece(square, Player::White);
        if let None = w {
            self.get_piece(square, Player::Black)
        } else {
            w
        }
    }
    fn iter_pieces(&self, player: Player) -> PieceIter {
        PieceIter::new(player, &self.pieces[player.index()])
    }

    pub fn to_fen(&self) -> String {
        let mut fen = String::new();
        for rank in 0..8 {
            let mut empty_count = 0;
            for file in 0..8 {
                let square = rank * 8 + file;
                let (piece, player) = match self.get_piece(square, Player::White) {
                    Some(p) => (Some(p), Player::White),
                    None => (self.get_piece(square, Player:: Black), Player::Black)
                };
                match piece {
                    Some(p) => {
                        let char = p.get_letter(player);
                        if empty_count != 0 {
                            fen += format!("{empty_count}").as_ref();
                        }
                        fen.push(char);
                        empty_count = 0;
                    }
                    None => {
                        empty_count += 1;
                    }
                }
            }
            if empty_count != 0 {
                fen += format!("{empty_count}").as_ref();
            }
            if rank != 7 {
                fen.push('/');
            }
        }
        fen.push(' ');
        
        let active_player = match self.active_player() {
            Player::Black => 'b',
            Player::White => 'w'
        };
        fen.push(active_player);
        fen.push(' ');

        let white_castles = match self.lost_castle_rights[Player::White.index()].get_rights() {
            CastleRights::None => "",
            CastleRights::KingSide => "K",
            CastleRights::QueenSide => "Q",
            CastleRights::Both => "KQ",
        };
        let black_castles = match self.lost_castle_rights[Player::Black.index()].get_rights() {
            CastleRights::None => "",
            CastleRights::KingSide => "k",
            CastleRights::QueenSide => "q",
            CastleRights::Both => "kq",
        };
        if white_castles == "" && black_castles == "" {
            fen.push('-');
        }
        fen += white_castles;
        fen += black_castles;
        fen.push(' ');

        match self.en_passant_target {
            Some(square) => {
                fen += moves::to_algerbraic(square).as_str();
            }
            None => {
                fen.push('-')
            }
        }
        fen.push(' ');
    
        fen += format!("{}", self.half_move - self.last_irreversable - 1).as_str();
        fen.push(' ');
        fen += format!("{}", self.half_move / 2 + self.half_move % 2).as_str();
        
        fen
    }

    #[allow(unused)]
    fn fmv(&self, from: u8, to: u8, promote: Option<Piece>) -> String {
        let res = match promote {
            None => format!("{}{}({})",pretty_square(from), pretty_square(to), self.half_move),
            Some(piece) => format!("{}{}{}({})", pretty_square(from), pretty_square(to), piece.get_char(Player::White), self.half_move)
        };
        res
        
    }
    pub fn get_hash(&self) -> HashT {
        self.hash
    }
    fn draw_board(buffer: &[char; 64], player: Player, last_move: Option<u8>) {
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
                let square = (i*8 + j) as u8;
                let spot = buffer[(i*8 + j) as usize];
                let highlight = match last_move {
                    Some(n) =>if n == square {'|'} else {' '},
                    None => ' ',
                };
                if is_black {
                    print!("{}{}{highlight}{}{highlight}", color::Bg(color::Black),color::Fg(color::White), spot)
                }
                else {
                    print!("{}{}{highlight}{}{highlight}", color::Bg(color::LightBlack),color::Fg(color::White), spot)
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
    pub fn show(&self, last_move: Option<u8>) {
        let mut buffer = [' '; 64];
        for p in self.iter_pieces(Player::White).chain(self.iter_pieces(Player::Black)) {  
            buffer[p.square as usize] = p.piece.get_char(p.player);
        }
        Board::draw_board(&buffer, self.active, last_move);
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

    fn king_pos(&self, player: Player) -> u8 {
        bit_set::lsb_pos(self.get_set(player, Piece::King))
    }

    fn check_en_passant(&self, slide: &SliderMasks, candidate: MoveSquares) -> bool {
        let enemy_pawn = adjust_square(candidate.to, 0, match self.active {
            Player::White => 1,
            Player::Black => -1,
        });
        if let Some(_) = self.get_piece_any(candidate.to) {
            return false;
        }
        let king_pos = self.king_pos(self.active);
        //check for checks
        let checks = self.cache.as_ref().unwrap().check;
        let target_check = bit_set::intersect(checks, bit_set::from_idx(enemy_pawn));

        if bit_set::intersect(bit_set::from_idx(candidate.from), self.cache.as_ref().unwrap().pinned) != 0 {
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
                bit_set::difference(bit_set::from_idx(x), a)
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
        match self.get_piece(rook_pos, self.active)  {
            Some(r) => {
                if r != Piece::Rook {
                    self.lost_castle_rights[self.active.index()].lose(match side {
                        CastleType::KingSide => CastleRights::KingSide,
                        CastleType::QueenSide => CastleRights::QueenSide,
                    });
                    return false;
                }
            },
            None => {
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
        let ss = bit_set::from_idx(square);
        let new_square = candidate.to;
        let nss = bit_set::from_idx(new_square);
        let piece = match self.get_piece(square, self.active) {
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
        let s = slide.get(square, new_square);
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
                        bit_set::from_idx(pinner), 
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
        match piece {
            Piece::King => {
                let new_attackers = self.get_square_attackers(new_square, gen);
                let is_attacked = bit_set::iter_pos(new_attackers).any(|attacker_pos|{
                    let attack = slide.get(new_square, attacker_pos);
                    let blockers = bit_set::intersect(bit_set::difference(bit_set::from_idx(square), all_pieces), attack);
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
                        let capture = bit_set::intersect(nss, bit_set::from_idx(check_pos));
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

    pub fn active_player(&self) -> Player {
        self.active
    }

    pub fn generate_legal(&mut self, gen: &PossibleMoveGenerator, slide: &SliderMasks) -> Vec<Move> {
        ////info!("---starting generation for half move {}----", self.half_move);
        let mut res: Vec<Move> = Vec::new();
        self.create_cache(gen, slide);
        let enemies = self.cache.as_ref().unwrap().enemies;
        let friends = self.cache.as_ref().unwrap().friends;
        for piece in self.iter_pieces(self.active) {

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
                                None => MoveType::Capture(self.get_piece(square, self.active.invert()).unwrap()),
                                Some(p) => MoveType::CaptureAndPromotion(self.get_piece(square, self.active.invert()).unwrap(), p)
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
                    if self.check_en_passant(slide, MoveSquares {
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
    pub fn generate_legal_captures(&mut self, gen: &PossibleMoveGenerator, slide: &SliderMasks) -> Vec<Move> {
        let mut res: Vec<Move> = Vec::new();
        self.create_cache(gen, slide);
        let enemies = self.cache.as_ref().unwrap().enemies;
        for piece in self.iter_pieces(self.active) {

            let possible_captures = gen.get_attacks(&piece);
            let captures = bit_set::intersect(enemies, possible_captures);

            for &promote in Self::generate_promotions(&piece){
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
                                None => MoveType::Capture(self.get_piece(square, self.active.invert()).unwrap()),
                                Some(p) => MoveType::CaptureAndPromotion(self.get_piece(square, self.active.invert()).unwrap(), p)
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
                    if self.check_en_passant(slide, MoveSquares {
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
        res
    }
    // DOES NOT CHECK IF MOVE IS LEGAL, panics if an empty square is moved.
    pub fn make_move(&mut self, m: &Move, hasher: &GameHasher) {
        let piece = match self.get_piece(m.from, self.active) {
            Some(n) => n,
            None => {
                panic!("move {:?} attempted, but no {} piece is on {}", 
                    m, 
                    match self.active {
                        Player::White => "white",
                        Player::Black => "black",
                    }, 
                    m.from
                );
            }
        };
        let set = self.get_set_mut(self.active, piece);
        bit_set::unset(set, m.from);
        bit_set::set(set, m.to);

        match m.move_type {
            MoveType::Capture(cap_piece) => {
                // if let Piece::King = piece {
                //     println!("{:?}", m);
                //     panic!("King can be captured!");
                // }
                match bit_set::get(self.pieces[self.active.invert().index()][cap_piece.index()], m.to) {
                    0 => panic!("move failed, no piece to capture"),
                    _ => {
                        let set = self.get_set_mut(self.active.invert(), cap_piece);
                        bit_set::unset(set, m.to);
                        self.hash ^= hasher.get_piece(&SpecefiedPiece { 
                            player: self.active.invert(), 
                            piece: cap_piece, 
                            square: m.to 
                        });
                        self.hash ^= hasher.get_piece(&SpecefiedPiece {
                            player: self.active,
                            piece: piece,
                            square: m.from,
                        });
                        self.hash ^= hasher.get_piece(&SpecefiedPiece {
                            player: self.active,
                            piece: piece,
                            square: m.to,
                        });
                        true
                    },
                };
                self.last_irreversable = self.half_move;
            },
            MoveType::EnPassent => {
                let square = adjust_square(m.to, 0, -self.active.pawn_dir(1));
                bit_set::unset(self.get_set_mut(self.active.invert(), Piece::Pawn), square);

                self.hash ^= hasher.get_piece(&SpecefiedPiece {
                    player: self.active.invert(),
                    piece: piece,
                    square: square,
                });
                self.hash ^= hasher.get_piece(&SpecefiedPiece {
                    player: self.active,
                    piece: piece,
                    square: m.from,
                });
                self.hash ^= hasher.get_piece(&SpecefiedPiece {
                    player: self.active,
                    piece: piece,
                    square: m.to,
                });

            }
            MoveType::Quiet => {
                self.hash ^= hasher.get_piece(&SpecefiedPiece {
                    player: self.active,
                    piece: piece,
                    square: m.from,
                });
                self.hash ^= hasher.get_piece(&SpecefiedPiece {
                    player: self.active,
                    piece: piece,
                    square: m.to,
                });
            }
            MoveType::Castle(side) => {
                let rook_square = Self::get_castle_rook(self.active, &side);
                let to = (m.from + m.to) / 2;
                bit_set::unset(self.get_set_mut(self.active, Piece::Rook), rook_square);
                bit_set::set(self.get_set_mut(self.active, Piece::Rook), to);

                //rook
                self.hash ^= hasher.get_piece(&SpecefiedPiece {
                    player: self.active,
                    piece: Piece::Rook,
                    square: rook_square,
                });
                self.hash ^= hasher.get_piece(&SpecefiedPiece {
                    player: self.active,
                    piece: Piece::Rook,
                    square: to,
                });

                //king
                self.hash ^= hasher.get_piece(&SpecefiedPiece {
                    player: self.active,
                    piece: piece,
                    square: m.from,
                });
                self.hash ^= hasher.get_piece(&SpecefiedPiece {
                    player: self.active,
                    piece: piece,
                    square: m.to,
                });

                for r in self.lost_castle_rights[self.active.index()].lose(CastleRights::Both).iter() {
                    self.hash ^= hasher.get_castle(*r, self.active);
                }
            },
            MoveType::Promotion(promote) => {
                bit_set::unset(self.get_set_mut(self.active, Piece::Pawn), m.to);
                bit_set::set(self.get_set_mut(self.active, promote), m.to);
                
                self.hash ^= hasher.get_piece(&SpecefiedPiece {
                    player: self.active,
                    piece: piece,
                    square: m.from,
                });
                self.hash ^= hasher.get_piece(&SpecefiedPiece {
                    player: self.active,
                    piece: promote,
                    square: m.to,
                });

                self.last_irreversable = self.half_move;

            }
            MoveType::CaptureAndPromotion(cap_piece, promote) => {
                bit_set::unset(self.get_set_mut(self.active, Piece::Pawn), m.to);
                bit_set::set(self.get_set_mut(self.active, promote), m.to);
                match bit_set::get(self.pieces[self.active.invert().index()][cap_piece.index()], m.to) {
                    0 => panic!("move failed, no piece to capture"),
                    _ => {
                        let set = self.get_set_mut(self.active.invert(), cap_piece);
                        bit_set::unset(set, m.to);

                        self.hash ^= hasher.get_piece(&SpecefiedPiece {
                            player: self.active,
                            piece: piece,
                            square: m.from,
                        });
                        self.hash ^= hasher.get_piece(&SpecefiedPiece {
                            player: self.active,
                            piece: promote,
                            square: m.to,
                        });

                        self.hash ^= hasher.get_piece(&SpecefiedPiece { 
                            player: self.active.invert(), 
                            piece: cap_piece, 
                            square: m.to 
                        });

                        true
                    },
                };
                self.last_irreversable = self.half_move;
            }
        }
        match self.en_passant_target {
            Some(ept) => {
                self.hash ^= hasher.get_en_passant(ept)
            }
            None => ()
        }
        self.en_passant_target = None;
        match piece{
            Piece::Pawn => {
                self.last_irreversable = self.half_move;
                if (rank(m.to) as i32 - rank(m.from) as i32).abs() == 2 {
                    let ept = (m.to + m.from) / 2;
                    self.en_passant_target = Some( ept ); //really stupid to do it like this
                    self.hash ^= hasher.get_en_passant(ept);
                }
            }
            Piece::King => {
                for r in self.lost_castle_rights[self.active.index()].lose(CastleRights::Both).iter() {
                    self.hash ^= hasher.get_castle(*r, self.active);
                }
            },
            Piece::Rook => {
                if m.from == Self::get_castle_rook(self.active, &CastleType::KingSide) {
                    for r in self.lost_castle_rights[self.active.index()].lose(CastleRights::KingSide).iter() {
                        self.hash ^= hasher.get_castle(*r, self.active);
                    }
                }
                else if m.from == Self::get_castle_rook(self.active, &CastleType::QueenSide) {
                    for r in self.lost_castle_rights[self.active.index()].lose(CastleRights::QueenSide).iter() {
                        self.hash ^= hasher.get_castle(*r, self.active);
                    }                
                }
            }
            _ => (),
        }
        self.half_move += 1;
        self.hash ^= hasher.get_player(self.active);
        self.active = self.active.invert();
        self.hash ^= hasher.get_player(self.active);
    }

    pub fn eval(&self) -> EvalT {
        let eval_player = |player| {
            Piece::iter().fold(0, |a, p| {
                let mut acc = 0;
                let mut set = self.get_set(player, p);
                while set != 0 {
                    let pos = 63 - bit_set::lsb_pos(set);
                    acc += eval_piece(player, p, pos);
                    bit_set::clear_lsb(&mut set);
                }
                acc + a
            })
        };
        eval_player(self.active_player()) - eval_player(self.active_player().invert())
    }

    fn perft_impl(&mut self, n:u32, gen: &PossibleMoveGenerator, slide: &SliderMasks, hasher: &GameHasher) -> u64 {
        if n == 0 {
            return 1;
        }
        let mut res = 0;
        let moves = self.generate_legal(gen, slide);
        for m in &moves {
            let mut b = self.clone();
            b.make_move(m, hasher);
            res += b.perft_impl(n - 1, gen, slide, hasher);
        };
        res
    }

    pub fn perft(&self, n:u32, gen: &PossibleMoveGenerator, slide: &SliderMasks, hasher: &GameHasher) -> u64 {
        if n == 0 {
            return 1;
        }
        let mut res = 0;
        let mut copy = self.clone();
        let moves = copy.generate_legal(gen, slide);
        for m in &moves {
            let mut b = copy.clone();
            b.make_move(m, hasher);
            let res2 = b.perft_impl(n - 1, gen, slide, hasher);
            println!("{}{}: {}", pretty_square(m.from), pretty_square(m.to), res2);
            res += res2;
        };
        res
    }
    pub fn in_check(&mut self, gen: &PossibleMoveGenerator, slide: &SliderMasks) -> bool {
        match &self.cache {
            None => {
                self.create_cache(gen, slide);
                if let Some(cache) = self.cache.as_ref() {
                    cache.check != 0
                }
                else { //unreachable code
                    false
                }
            },
            Some(cache) => {
                cache.check != 0
            }
        }
    }

    fn new_hash(&self, hasher: &GameHasher) -> HashT {
        let mut res = 0;
        for p in self.iter_pieces(Player::Black).chain(self.iter_pieces(Player::White)) {
            res ^= hasher.get_piece(&p);
        }
        res ^= hasher.get_player(self.active);
        for player in Player::iter() {
            for right in self.lost_castle_rights[player.index()].get_rights().iter() {
                res ^= hasher.get_castle(*right, player);
            }
        }
        match self.en_passant_target {
            Some(n) => res ^ hasher.get_en_passant(n),
            None => res
        }
    }

    pub fn update_hash(mut self, hasher: &GameHasher) -> Self {
        self.hash = self.new_hash(hasher);
        self
    }
}

struct PieceIter<'a> {
    all: Set,
    player: Player,
    pieces: &'a [Set; 6],
    pos: u8,
}

impl<'a> PieceIter<'a> {
    fn new(player: Player, pieces: &'a [Set; 6]) -> Self {
        let all = pieces.iter().fold(0u64, |a,x| bit_set::union(a, *x));
        Self { all, player, pieces, pos: 0 }
    }
    fn with_all_known(player: Player, pieces: &'a [Set; 6], all: Set) -> Self {
        Self { all, player, pieces, pos: 0 }
    }
}

impl<'a> Iterator for PieceIter<'a> {
    type Item = SpecefiedPiece;
    fn next(&mut self) -> Option<Self::Item> {
        for square in self.pos..64 {
            if bit_set::get(self.all, square) != 0 {
                for p in Piece::iter() {
                    let set = self.pieces[p.index()];
                    if bit_set::get(set, square) != 0 {
                        self.pos = square + 1;
                        return Some(SpecefiedPiece { player: self.player, piece: p, square });
                    }
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod test {
    use crate::chess::{moves::{Move, CastleType}, piece::{Piece}, board::{Board, adjust_square}, generator::{PossibleMoveGenerator, SliderMasks}, hash::GameHasher};

    use super::MoveType;
    #[test]
    fn file_and_rank() {
        assert!( super::file(32u8) == 0);
        assert!( super::rank(32u8) == 4);
        assert!(adjust_square(28, 1, 1) == 37);
    }
    #[test]
    fn get_square_attackers() {
        let gen = super::PossibleMoveGenerator::new();
        let hasher = super::GameHasher::new();
        let b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ", &hasher).unwrap();
        let square = super::adjust_square(0, 2, 3);
        let attackers = b.get_square_attackers(square, &gen);
        println!("{} {attackers}", super::pretty_square(square));
        assert!(attackers == 0x80080000)
    }
    #[test]
    fn perft() {
        let depth = 4;
        let positions = [
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - ",
            "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ",
            "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
            "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8  ",
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
        ];
        let _nodes_5 = [ 
            4865609,
            193690690,
            674624,
            15833292,
            89941194,
            164075551,
        ];
        let nodes_4 = [
            197281 ,
            4085603 ,
            43238,
            422333,
            2103487,
            3894594,
        ];
        let gen = PossibleMoveGenerator::new();
        let slide = SliderMasks::new();
        let hasher = super::GameHasher::new();
        for (fen, nodes) in positions.iter().zip(nodes_4) {
            let b = Board::from_fen(fen, &hasher).unwrap();
            assert!(b.perft(depth, &gen, &slide, &hasher) == nodes);
        }
    }
    #[test]
    fn hash_move() {
        let hasher = GameHasher::new();
        let mut b1 = Board::new(&hasher);
        b1.make_move(&Move {
            from: 55,
            to: 39,
            move_type: MoveType::Quiet,
        }, &hasher);
        let b2 = Board::from_fen("rnbqkbnr/pppppppp/8/8/7P/8/PPPPPPP1/RNBQKBNR b KQkq h3 0 1", &hasher).unwrap();
        assert!(b1.hash == b2.hash);
    }
    #[test]
    fn hash_castle() {
        let hasher = GameHasher::new();
        let mut b1 = Board::from_fen("rnbqk1nr/ppp2ppp/3p4/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4", &hasher).unwrap();
        b1.make_move(&Move {
            from: 60,
            to: 62,
            move_type: MoveType::Castle(CastleType::KingSide),
        }, &hasher);
        let b2 = Board::from_fen("rnbqk1nr/ppp2ppp/3p4/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 1 4", &hasher).unwrap();
        assert!(b1.hash == b2.hash);
    }
    #[test]
    fn hash_capture() {
        let hasher = GameHasher::new();
        let mut b1 = Board::from_fen("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2", &hasher).unwrap();
        b1.make_move(&Move {
            from: 36,
            to: 27,
            move_type: MoveType::Capture(Piece::Pawn),
        }, &hasher);
        let b2 = Board::from_fen("rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2", &hasher).unwrap();
        assert!(b1.hash == b2.hash);
    }
    #[test]
    fn hash_en_passant() {
        let hasher = GameHasher::new();
        let mut b1 = Board::from_fen("rnbqkbnr/ppppp1p1/7p/4Pp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3", &hasher).unwrap();
        b1.make_move(&Move {
            from: 28,
            to: 21,
            move_type: MoveType::EnPassent,
        }, &hasher);
        let b2 = Board::from_fen("rnbqkbnr/ppppp1p1/5P1p/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3", &hasher).unwrap();
        assert!(b1.hash == b2.hash);
    }
    #[test]
    fn hash_diff() {
        let hasher = GameHasher::new();
        let mut b1 = Board::from_fen("rnbqkbnr/ppppp1p1/7p/4Pp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3", &hasher).unwrap();
        let hash = b1.hash;
        b1.make_move(&Move {
            from: 28,
            to: 21,
            move_type: MoveType::EnPassent,
        }, &hasher);
        assert!(hash != b1.hash);
    }}