use super::{
    piece::{Piece, Player},
    bit_set::{Set, self, union},
    board::{
        rank, file, self
    }
};
trait MoveGen {
    fn new() -> Self;
    fn init(&mut self);
    fn get_attacks<N>(&self, player: Player, square: N) -> Set
        where N: Into<usize> + Copy;
    fn get_moves<N>(&self, player: Player, square: N) -> Set
        where N: Into<usize>+ Copy;
}

struct PawnMoveGen {
    lookup_black_moves: [Set; 64],
    lookup_white_moves: [Set; 64],
    lookup_black_attacks: [Set; 64],
    lookup_white_attacks: [Set; 64],
}
struct KnightMoveGen {
    lookup: [Set; 64],
}
struct BishopMoveGen {
    lookup: [Set; 64],
}
struct RookMoveGen {
    lookup_rank: [Set; 8],
    lookup_file: [Set; 8],
}

struct QueenMoveGen {
    lookup: [Set; 64]
}

struct KingMoveGen {
    lookup: [Set; 64],
    castle_square: [usize; 2],
    castle_moves: [Set; 2],
}

pub struct PossibleMoveGenerator {
    pawn: PawnMoveGen,
    knight: KnightMoveGen,
    bishop: BishopMoveGen,
    rook: RookMoveGen,
    queen: QueenMoveGen,
    king: KingMoveGen,
}

impl PossibleMoveGenerator {
    pub fn new() -> Self{
        let mut pawn = PawnMoveGen::new();
        let mut knight = KnightMoveGen::new();
        let mut bishop = BishopMoveGen::new();
        let mut rook = RookMoveGen::new();
        let mut queen = QueenMoveGen::new();
        let mut king = KingMoveGen::new();
        pawn.init();
        knight.init();
        bishop.init();
        rook.init();
        queen.init();
        king.init();
        Self {
            pawn,
            knight,
            bishop,
            rook,
            queen,
            king
        }
    }
    pub fn get_moves<N>(&self, player: Player, piece: Piece, square: N) -> Set 
        where N: Into<usize> + Copy {
        match piece {
            Piece::Pawn =>   self.pawn.get_moves(player, square),
            Piece::Knight => self.knight.get_moves(player, square),
            Piece::Bishop => self.bishop.get_moves(player, square),
            Piece::Rook =>   self.rook.get_moves(player, square),
            Piece::Queen =>  self.queen.get_moves(player, square),
            Piece::King =>   self.king.get_moves(player, square)
        }
    }
    pub fn get_attacks<N>(&self, player: Player, piece: Piece, square: N) -> Set 
        where N: Into<usize> + Copy {
        match piece {
            Piece::Pawn =>   self.pawn.get_attacks(player, square),
            Piece::Knight => self.knight.get_attacks(player, square),
            Piece::Bishop => self.bishop.get_attacks(player, square),
            Piece::Rook =>   self.rook.get_attacks(player, square),
            Piece::Queen =>  self.queen.get_attacks(player, square),
            Piece::King =>   self.king.get_attacks(player, square)
        }
    }
}

impl MoveGen for PawnMoveGen {
    fn new() -> Self {
        Self {
            lookup_black_moves: [0; 64],
            lookup_black_attacks: [0; 64],
            lookup_white_moves: [0; 64],
            lookup_white_attacks: [0;64],
        }
    }
    fn init(&mut self) {
        //init white
        for ((m,a), square) in self.lookup_white_moves.iter_mut()
            .zip(self.lookup_white_attacks.iter_mut()).zip(0u8..) {
            if rank(square) > 6 || rank(square) < 1 {
                continue;
            }
            bit_set::set(m, board::adjust_square(square, 0, -1));
            if file(square) != 0 {
                bit_set::set(a, board::adjust_square(square, -1, -1));
            }
            if file(square) != 7 {
                bit_set::set(a, board::adjust_square(square, 1, -1));
            }
            if rank(square) ==  6 {
                bit_set::set(m, board::adjust_square(square, 0, -2));
            }
        }
        for ((m,a), square) in self.lookup_black_moves.iter_mut()
            .zip(self.lookup_black_attacks.iter_mut()).zip(0u8..) {
            if rank(square) < 1 || rank(square) > 6 {
                continue;
            }
            bit_set::set(m, board::adjust_square(square, 0, 1));
            if file(square) != 0 {
                bit_set::set(a, board::adjust_square(square, -1, 1));
            }
            if file(square) != 7 {
                bit_set::set(a, board::adjust_square(square, 1, 1));
            }
            if rank(square) ==  1 {
                bit_set::set(m, board::adjust_square(square, 0, 2));
            }
        }
    }
    fn get_attacks<N>(&self, player: Player, square: N) -> Set 
        where N: Into<usize>+ Copy {
        match player {
            Player::White => self.lookup_white_attacks[square.into()],
            Player::Black => self.lookup_black_attacks[square.into()],
        }
    }

    fn get_moves<N>(&self, player: Player, square: N) -> Set
        where N: Into<usize>+ Copy {
        match player {
            Player::White => self.lookup_white_moves[square.into()],
            Player::Black => self.lookup_black_moves[square.into()],
        }
    }

}

impl MoveGen for KnightMoveGen {
    fn new() -> Self {
        Self {
            lookup: [0; 64],
        }
    }
    fn init(&mut self) {
        let pattern = [
            (2,1),(2,-1),(1,2),(1,-2),(-2,1),(-2,-1),(-1,-2),(-1,2)
        ];
        for (set,square) in &mut self.lookup.iter_mut().zip(0u8..){
            for (file, rank) in pattern {
                let move_square = match board::adjust_square_checked(square, file, rank) {
                    Some(n) => n,
                    None => {continue;}
                };
                bit_set::set(set, move_square);
            }

        }
    }

    fn get_attacks<N>(&self, player: Player, square: N) -> Set
        where N: Into<usize>+ Copy {
        self.get_moves(player, square)
    }

    fn get_moves<N>(&self, _: Player, square: N) -> Set
        where N: Into<usize>+ Copy {
        self.lookup[square.into()]
    }
}

impl MoveGen for BishopMoveGen {
    fn new() -> Self {
        Self {
            lookup: [0; 64],
        }
    }
    fn init(&mut self) {
        for (set, square) in self.lookup.iter_mut().zip(0u8..) {
            
            let dirs:[(i32,i32); 4] = [(1,1), (-1,1), (1,-1), (-1,-1)];
            for (df, dr) in dirs {
                for (nf, nr) in (0..8)
                    .map(|n| {(n*df, n*dr)}) 
                {
                    let move_square = match board::adjust_square_checked(square, nf, nr) {
                        Some(n) => n,
                        None => {break},
                    };
                    bit_set::set(set, move_square);
                }
            }

            bit_set::unset(set, square);
        }
    }

    fn get_attacks<N>(&self, player: Player, square: N) -> Set
        where N: Into<usize>+ Copy {
        self.get_moves(player, square)
    }

    fn get_moves<N>(&self, _: Player, square: N) -> Set
        where N: Into<usize>+ Copy {
        self.lookup[square.into()]
    }
}

impl MoveGen for RookMoveGen {
    fn new() -> Self {
        Self {
            lookup_file: [0; 8],
            lookup_rank: [0; 8],
        }
    }
    fn init(&mut self) {
        let starter_file: Set = 0x0101010101010101u64;
        for (set, file) in self.lookup_file.iter_mut().zip(0u8..) {
            *set = starter_file << file
        }
        let starter_rank: Set = 0xffu64;
        for (set, rank) in self.lookup_rank.iter_mut().zip(0u8..) {
            *set = starter_rank << (rank * 8);
        }
    }

    fn get_attacks<N>(&self, player: Player, square: N) -> Set
        where N: Into<usize> + Copy{
        self.get_moves(player, square)
    }

    fn get_moves<N>(&self, _player: Player, square: N) -> Set
        where N: Into<usize> + Copy{
        bit_set::xor(
            self.lookup_file[file(square.into())], 
            self.lookup_rank[rank(square.into())]
        )
    }
}

impl MoveGen for QueenMoveGen {
    fn new() -> Self {
        Self {
            lookup: [0; 64],
        }
    }
    fn init(&mut self) {
        let rook_file = 0x0101010101010101u64;
        let rook_rank = 0xffu64;
        for (set, square) in self.lookup.iter_mut().zip(0u8..) {
            let dirs:[(i32,i32); 4] = [(1,1), (-1,1), (1,-1), (-1,-1)];
            for (df, dr) in dirs {
                for (nf, nr) in (0..8)
                    .map(|n| {(n*df, n*dr)}) 
                {
                    let move_square = match board::adjust_square_checked(square, nf, nr) {
                        Some(n) => n,
                        None => {break},
                    };
                    bit_set::set(set, move_square);
                }
            }
            let file = file(square);
            let rank = rank(square);
            let rook_attack = bit_set::union(rook_rank << (rank * 8), rook_file << file);
            *set = union(*set, rook_attack);
            bit_set::unset(set, square);
        }
    }

    fn get_attacks<N>(&self, player: Player, square: N) -> Set
        where N: Into<usize> + Copy{
        return self.get_moves(player, square);
    }

    fn get_moves<N>(&self, _player: Player, square: N) -> Set
        where N: Into<usize> + Copy{
        self.lookup[square.into()]
    }
}

impl MoveGen for KingMoveGen {
    fn new() -> Self {
        Self {
            castle_moves: [0x4400000000000000, 0x44],
            castle_square: [0x1000000000000000, 0x10],
            lookup: [0; 64],

        }
    }
    fn init(&mut self) {
        let dirs = [
            (1,1), (0,1), (-1,1), (-1,0),(-1,-1),(0,-1),(1,-1),(1,0),
        ];
        for (set, square) in self.lookup.iter_mut().zip(0u8..) 
        {
            for (df, dr) in dirs {
                match board::adjust_square_checked(square, df, dr) {
                    Some(move_square) => {
                        bit_set::set(set, move_square);
                    }
                    None => {
                        continue;
                    }
                }
            }
        }
    }

    fn get_attacks<N>(&self, _player: Player, square: N) -> Set
        where N: Into<usize> + Copy{
        self.lookup[square.into()]
    }

    fn get_moves<N>(&self, player: Player, square: N) -> Set
        where N: Into<usize> + Copy{
        let bool_multi = match square.into() == self.castle_square[player.index()] {
            true => 1,
            false => 0,
        };
        bit_set::union(self.lookup[square.into()], bool_multi * self.castle_moves[player.index()])
    }
}