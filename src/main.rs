mod chess;
use std::{env};
use chess::MoveResult;
use log4rs;
use std::io;

enum Command {
    Perft(u32),
    Move(String),
    Fen(String),
    Show,
    ShowFen,
}

fn get_user_input() -> Option<String> {
    let mut res = String::new();
    match io::stdin().read_line(&mut res) {
        Ok(bytes) => {
            if bytes == 0 {
                None
            }
            else {
                Some(res)
            }
        }
        Err(e) =>{
            eprintln!("Error reading input: {e}");
            None
        }
    }
}
fn transform_input(input: String) -> Result<Command, String> {
    let mut chiter = input.trim().chars().map(|c| c.to_ascii_lowercase());
    let command: String = chiter.by_ref().take_while(|c| {
        !c.is_whitespace()
    }).collect();
    let args: String = chiter.collect();
    match command.as_str() {
        "perft" => {
            let n = match u32::from_str_radix(&args, 10) {
                Ok(n) => n,
                Err(_) => {return Err(format!("Invalid perft depth: {args}"))}
            };
            Ok(Command::Perft(n))
        },
        "fen" => {
            match args.as_str() {
                "show" => Ok(Command::ShowFen),
                _ => Ok(Command::Fen(args)),
            }
        },
        "show" => {
            Ok(Command::Show)
        },
        _ => {
            Ok(Command::Move(command))
        }
    }

}
fn main() {
    if let Err(e) =  log4rs::init_file("./log4rs.yml", Default::default()) {
        panic!("{}", e);
    }
    let args: Vec::<String> = env::args().collect();
    let fen = args.get(1);
    let mut game = match fen {
        Some(f) => {
            chess::Game::from_fen(f)
        },
        None => chess::Game::new()
    };
    game.show();
    loop {
        let user_input = match get_user_input() {
            Some(n) => n,
            None => {break;}
        };
        let command = match transform_input(user_input) {
            Ok(c) => c,
            Err(msg) => {
                eprintln!("{msg}");
                continue;
            }
        };
        match command {
            Command::Perft(n) => {
                game.perft(n);
            },
            Command::Move(m) => {
                match game.try_move(&m) {
                    MoveResult::Win => {
                        println!("Checkmate! {} wins", match game.active_player() {
                            chess::Player::Black => "Black",
                            chess::Player::White => "White",
                        });
                        break;
                    },
                    MoveResult::Draw => {
                        println!("Stalemate! It's a draw");
                        break;
                    },
                    MoveResult::NextTurn => {
                        game.show();
                    },
                    MoveResult::InvalidInput => {
                        eprintln!("Invalid move, use UCI move notation (file)(rank)(file)(rank)[promote to]");
                        continue;
                    },
                    MoveResult::InvalidMove => {
                        println!("Illegal move!");
                        continue;
                    },

                }
            },
            Command::Fen(fen) => {
                game = chess::Game::from_fen(&fen);
            },
            Command::ShowFen => {
                println!("Fen: {}", game.fen())
            }
            Command::Show => {
                game.show();
            }
        }
    }
}

