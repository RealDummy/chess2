use super::piece::{SpecefiedPiece, Player};
use super::board::{CastleType, file};
use tinyrand;
use tinyrand::Rand;

pub type HashT = u64;
pub struct GameHasher {
    piece: [[HashT; 64]; 12],
    player: HashT,
    castle_rights: [HashT; 4],
    en_passant_file: [HashT; 8],
}

impl GameHasher {
    pub fn new() -> Self {
        let mut rand = tinyrand::StdRand::default();
        let mut piece = [[0; 64]; 12];
        piece.iter_mut().flatten().for_each(|x| *x = rand.next_u64());
        let player = rand.next_u64();
        let mut castle_rights = [0; 4];
        castle_rights.iter_mut().for_each(|x| *x = rand.next_u64());
        let mut en_passant_file = [0; 8];
        en_passant_file.iter_mut().for_each(|x| *x = rand.next_u64());
        Self {
            piece,
            player,
            castle_rights,
            en_passant_file,
        }
    }
    pub fn get_piece(&self, sp: &SpecefiedPiece) -> HashT {
        let index = sp.player.index() * 6 + sp.piece.index();
        self.piece[index][sp.square as usize]
    }
    pub fn get_player(&self, player: Player) -> HashT {
        self.player ^ match player {
            Player::White => self.player,
            Player::Black => 0,
        }
    }
    pub fn get_castle(&self, right: CastleType, player: Player) -> HashT {
        let right_index: usize = match right {
            CastleType::KingSide => 0,
            CastleType::QueenSide => 1,
        };
        let player_index = 2 * player.index();
        self.castle_rights[right_index + player_index]
    }
    pub fn get_en_passant(&self, ept: u8) -> HashT {
        self.en_passant_file[file(ept) as usize]
    }
}