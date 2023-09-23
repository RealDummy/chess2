mod chess;
use std::{env, time::{Duration, Instant}};
use chess::MoveResult;
use log4rs;
use std::io;

enum Command {
    Perft(u32),
    Move(String),
    Fen(String),
    Show,
    ShowFen,
    Exit,
    Reset,
    Help,
    Switch,
    Bench(u8),
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
    let mut chiter = input.trim().chars();
    let mut command: String = chiter.by_ref().take_while(|c| {
        !c.is_whitespace()
    }).map(|c| c.to_ascii_lowercase()).collect();
    let args: String = chiter.collect();
    command.make_ascii_lowercase();
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
        "quit" | "exit" => {
            Ok(Command::Exit)
        },
        "reset" => {
            Ok(Command::Reset)
        }
        "help" => {
            Ok(Command::Help)
        }
        "switch" => {
            Ok(Command::Switch)
        }
        "bench" => {
            let depth = match u8::from_str_radix(&args, 10) {
                Ok(d) => d,
                Err(_) => { 
                    return Err("invalid depth".to_owned()); 
                }
            };
            Ok(Command::Bench(depth))
        }
        _ => {
            Ok(Command::Move(command))
        }
    }

}
fn print_move_result(player: chess::Player, mr: &MoveResult) -> Option<chess::Player> {
    match mr {
        MoveResult::Win => {
            println!("Checkmate! {} wins", match player {
                chess::Player::Black => "White",
                chess::Player::White => "Black",
            });
            None
        }
        MoveResult::Draw => {
            println!("Stalemate! It's a draw");
            None
        }
        MoveResult::NextTurn => {
            Some(player.invert())
        }
        MoveResult::InvalidInput => {
            eprintln!("Invalid move, use UCI move notation <file><rank><file><rank>[promote]");
            Some(player)
        }
        MoveResult::InvalidMove => {
            println!("Illegal move!");
            Some(player)
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
            chess::Game::from_fen(f).expect("invalid fen")
        },
        None => chess::Game::new()
    };
    game.show();
    let mut user = game.active_player();
    loop {
        if game.active_player() != user {
            let best = game.find_best_move_timed(Duration::from_secs(1));
            let res = game.try_move(&best);
            game.show();
            if let None = print_move_result(game.active_player(), &res) {
                break;
            }
        }
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
                let uci = match game.read_uci(&m) {
                    Ok(m) => m,
                    Err(msg) => {
                        eprintln!("{msg}");
                        continue;
                    }
                };
                let mr = game.try_move(&uci);
                game.show();
                if let None = print_move_result(game.active_player(), &mr) {
                    break;
                }
            },
            Command::Fen(fen) => {
                game = chess::Game::from_fen(&fen).or_else(|e| {
                    eprintln!("{e}");
                    Ok::<chess::Game, &'static str>(game)
                }).unwrap();
                user = game.active_player();
            },
            Command::ShowFen => {
                println!("Fen: {}", game.fen())
            }
            Command::Show => {
                game.show();
            }
            Command::Reset => {
                game = chess::Game::new();
                user = game.active_player();
            }
            Command::Switch => {
                user = user.invert();
            }
            Command::Help => {
                let info = include_str!("help.txt");
                println!("{info}");
            }
            Command::Exit => {
                break;
            }
            Command::Bench(depth) => {
                let iterations = 10;
                let mut time = Duration::new(0, 0);
                let mut best = None;
                for _ in 0..iterations {
                    game.clear_data();
                    let begin = Instant::now();
                    best = Some(game.find_best_move_depth(2, depth));
                    let end = Instant::now();
                    time += end - begin;
                }
                println!("move {:?} found in about {}ms", best.unwrap(), (time / iterations).as_millis());
            }
        }
    }
}

