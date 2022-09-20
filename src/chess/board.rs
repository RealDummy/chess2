use std::convert::TryInto;
use std::vec::Vec;

use super::input;
use crate::chess::piece::{Piece, BLACK_START, WHITE_START, Player, SpecefiedPiece};
use crate::chess::bit_set;
pub use super::moves::{Move, MoveType, MoveSquares};
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
    let a: u8 = 'A' as u8;
    (a + file).into()
}
fn pretty_rank(rank: u8) -> char {
    let eight: u8 = '8' as u8;
    (eight - rank).into()

}
fn pretty_square(square: u8) -> String {
    format!("{}{}", pretty_file(file(square)), pretty_rank(rank(square)))
}   

fn fmv(from: u8, to: u8, promote: Option<Piece>) -> String {
    match promote {
        None => format!("{}{}",pretty_square(from), pretty_square(to)),
        Some(piece) => format!("{}{}{}", pretty_square(from), pretty_square(to), piece.get_char(Player::White))
    }
    
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
    fn get_set(&self, player: Player, piece: Piece) -> Set {
        self.pieces[player.index()][piece.index()]
    }
    fn get_set_mut<'a>(&'a mut self, player: Player, piece: Piece) -> &'a mut Set{
        &mut self.pieces[player.index()][piece.index()]
    }
    // pub fn iter_pieces<'a>(&'a self) -> MailboxIter<'a> {
    //     self.mailbox.iter()
    // }
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
    fn create_cache(&mut self, gen: &PossibleMoveGenerator, slide: &SliderMasks) {
        let opponent = self.active.invert(); 
        info!("current player: {:?}", self.active);
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
            if bit_set::count(pf) == 1 {
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

    fn check_castle(&self, gen: &PossibleMoveGenerator, slide: &SliderMasks, candidate: MoveSquares) -> bool {
        todo!();
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
                info!("move {} rejected: no piece there", fmv(square, new_square, None));
                return false;
            }
        };
        if bit_set::intersect(friends, nss) != 0 {
            info!("move {} rejected: lands on friend", fmv(square, new_square, None));
            return false;
        }
        let s = slide.get(piece.square, new_square);
        if bit_set::intersect(s, all_pieces) != 0 {
            info!("move {} rejected: slide blocked", fmv(square, new_square, None));
            return false;
        }
        if bit_set::intersect(ss, pinned) != 0{
            if bit_set::intersect(nss, pinners) == 0 {
                info!("move {} rejected: piece is pinned", fmv(square, new_square, None));
                return false;
            }
        }
        let king_pos = bit_set::lsb_pos(self.get_set(self.active, Piece::King));
        match piece.piece {
            Piece::King => {
                let new_attackers = self.get_square_attackers(new_square, gen);
                let is_attacked = bit_set::iter_pos(new_attackers).any(|attacker_pos|{
                    let attack = slide.get(new_square, attacker_pos);
                    let blockers = bit_set::intersect(all_pieces, attack);
                    bit_set::count(blockers) == 0
                });
                if is_attacked {
                    info!("move {} rejected: king moved into check", fmv(piece.square, new_square, None));
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
                        info!("move {} rejected: king still in check", fmv(piece.square, new_square, None));
                        return false;
                    }
                }
            }
        }
        true
    }


    pub fn generate_legal(&mut self, gen: &PossibleMoveGenerator, slide: &SliderMasks) -> Vec<Move> {
        info!("---starting generation for half move {}----", self.half_move);
        let mut res: Vec<Move> =vec![];
        self.create_cache(gen, slide);
        let enemies = self.cache.as_ref().unwrap().enemies;
        let friends = self.cache.as_ref().unwrap().friends;
        for piece in self.mailbox.iter().filter(|p| p.player == self.active) {
            let possible_moves = bit_set::difference(friends, gen.get_moves(&piece));
            let quiet_moves = bit_set::difference(enemies, possible_moves);
            for square in bit_set::iter_pos(quiet_moves) {
                let candidate = MoveSquares {
                    from: piece.square,
                    to: square,
                    promote: None,
                };
                if self.check_move_unchecked(gen, slide, candidate) {
                    res.push(Move {
                        from: piece.square,
                        to: square,
                        move_type: MoveType::Quiet,
                    })
                }
            }
            let possible_captures = bit_set::difference(friends, gen.get_attacks(&piece));
            let captures = bit_set::intersect(enemies, possible_captures);
            for square in bit_set::iter_pos(captures) {
                let candidate = MoveSquares {
                    from: piece.square,
                    to: square,
                    promote: None,
                };
                if self.check_move_unchecked(gen, slide, candidate) {
                    res.push(Move {
                        from: piece.square,
                        to: square,
                        move_type: MoveType::Capture(self.mailbox.get(square).unwrap().piece)
                    })
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
                    if self.check_en_passant(gen, slide, MoveSquares {
                        to: ept,
                        from: square,
                        promote: None,
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

    //DOES NOT CHECK IF MOVE IS LEGAL, panics if an empty square is moved.
    pub fn make_move(&mut self, m: &MoveSquares) {
        let piece = match self.mailbox.get(m.from) {
            Some(n) => n,
            None => {panic!("move {:?} attempted, but no piece is on {}", m, m.from);}
        };
        let set = self.get_set_mut(piece.player, piece.piece);
        bit_set::unset(set, m.from);
        bit_set::set(set, m.to);
        let capture = match self.mailbox.force_move(m) {
            Some(cap) => {
                let set = self.get_set_mut(cap.player, cap.piece);
                bit_set::unset(set, m.to);
                true
            },
            None => false,
        };


        self.en_passant_target = None;
        if capture {
            self.last_irreversable = self.half_move;
        }
        else if Piece::Pawn == piece.piece {
            self.last_irreversable = self.half_move;
            if (rank(m.to) as i32 - rank(m.from) as i32).abs() == 2 {
                self.en_passant_target = Some( (m.to + m.from) / 2 ); //really stupid to do it like this
            }
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
                        self.make_move(&MoveSquares {
                            from: m.from,
                            to: m.to,
                            promote: None,
                        })
                    }
                    None => {println!("Invalid Move (not in psuedolegal moves)"); continue;}
                }
                break;
            }
            
        }
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

    pub fn force_move(&mut self, m: &MoveSquares) -> Option<SpecefiedPiece> {
        let t = self.get(m.from)?;
        self.squares[m.from as usize] = None;
        let res = self.get(m.to);
        self.squares[m.to as usize] = Some(MailboxPiece {piece: t.piece, player: t.player});
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
    use crate::chess::board::adjust_square;

    #[test]
    fn file_and_rank() {
        assert!( super::file(32u8) == 0);
        assert!( super::rank(32u8) == 4);
        assert!(adjust_square(28, 1, 1) == 37);
    }
}