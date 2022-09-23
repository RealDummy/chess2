use crate::chess::moves;
use crate::chess::piece::Piece;
pub fn get_square(square: &str) -> Result<u8, String>{
    let mut square_iter = square.chars();
    let file = match square_iter.next() {
        Some(c) => match c.to_ascii_lowercase() {
            'a' => 0,
            'b' => 1,
            'c' => 2,
            'd' => 3,
            'e' => 4,
            'f' => 5,
            'g' => 6,
            'h' => 7,
            _   => {return Err(format!("unknown file {}" , c))}
        }
        None => {
            return Err("unexpected end of input".to_owned());
        }
    };
    let rank = match square_iter.next() {
        Some(c) => match c {
            '1' => 7,
            '2' => 6,
            '3' => 5,
            '4' => 4,
            '5' => 3,
            '6' => 2,
            '7' => 1,
            '8' => 0,
            _   => {return Err(format!("unknown rank {}" , c))}

        }
        None => {
            return Err("unexpected end of input".to_owned());
        }
    };
    Ok(rank * 8 + file)
}

pub fn read_uci(uci: &str) -> Result<moves::MoveSquares, String> {
    if uci.len() < 4 {
        return Err(format!("Invalid UCI: {}", uci.trim()));
    }
    let from = match get_square(&uci[0..2]) {
        Ok(from)     => from,
        Err(msg) => {return Err(format!("Invalid UCI: {}", msg));}
    };
    let to = match get_square(&uci[2..4]) {
        Ok(to)     => to,
        Err(msg) => {return Err(format!("Invalid UCI: {}", msg));}
    };
    let promote = if uci.len() > 4 {
        match uci[4..].chars().next() {
            Some(p) => match p.to_ascii_lowercase() {
                'q' => Some(Piece::Queen),
                'r' => Some(Piece::Rook),
                'k' => Some(Piece::Knight),
                'b' => Some(Piece::Bishop),
                _   => None,
            }
            None => None
        }
    } else {
        None
    };
    Ok(moves::MoveSquares {
        from,
        to,
        promote,
    })
}

pub fn get_input() -> String {
    let mut res = String::new();
    loop {
        if let Err(msg) =  std::io::stdin().read_line(&mut res) {
            eprintln!("Can't read input: {}", msg);
            continue;
        }
        break;
    }
    res
}

#[cfg(test)]
mod tests {
    use crate::chess::{moves::MoveSquares, piece::Piece};

    use super::read_uci;

    #[test]
    fn read_uci_normal() {
        let res = read_uci("e4e5");
        assert!(res.is_ok());
        let MoveSquares {
            from,
            to,
            promote,
        } = res.unwrap();
        assert!(to == 28);
        assert!(from == 36);
        if let Some(_) = promote {
            panic!()
        }
    }
    #[test]
    fn read_uci_promote() {
        let res = read_uci("e7e8r");
        assert!(res.is_ok());
        let MoveSquares {
            from,
            to,
            promote,
        } = res.unwrap();
        assert!(to == 4);
        assert!(from == 12);
        if let Some(r) = promote {
            assert!(r == Piece::Rook);
        } else {
            panic!()
        }
    }
    #[test]
    fn read_uci_garbage() {
        assert!(read_uci("123").is_err());
        assert!(read_uci("1234").is_err());
        let res = read_uci("e7e8m");
        assert!(res.is_ok());
        let MoveSquares {
            promote,
            ..
        } = res.unwrap();
        if let Some(_) = promote {
           panic!()
        }
    }
}