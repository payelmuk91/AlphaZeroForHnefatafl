#![cfg(feature = "demo")]

use hnefatafl::preset;
use std::io::stdin;
use std::str::FromStr;
use hnefatafl::game::GameOutcome::{Draw, Win};
use hnefatafl::game::GameStatus::Over;
use hnefatafl::game::{Game, SmallBasicGame};
use hnefatafl::play::Play;
use hnefatafl::game::tiles::Tile;

fn input(prompt: &str) -> std::io::Result<String> {
    println!("{prompt}");
    let mut s: String = String::new();
    stdin().read_line(&mut s)?;
    Ok(s.trim().to_string())
}

fn get_play() -> Play {
    loop {
        if let Ok(m_str) = input("Please enter your move:") {
            match Play::from_str(&m_str) {
                Ok(play) => return play,
                Err(e) => println!("Invalid move ({e:?}). Try again.")
            }
        } else {
            println!("Error reading input. Try again.");
        }
        
    }
}

fn get_all_possible_moves<T: BoardState>(game: &Game<T>) -> Vec<Play> {
    let mut possible_moves = Vec::new();
    for tile in game.state.board.iter_occupied(game.state.side_to_play) {
        if let Ok(mut iter) = game.iter_plays(tile) {
            while let Some(valid_play) = iter.next() {
                possible_moves.push(valid_play.play);
            }
        }
    }
    possible_moves
}

fn validate_moves<T: BoardState>(game: &Game<T>, possible_moves: Vec<Play>) -> Vec<u8> {
    possible_moves.iter().map(|play| {
        match game.logic.validate_play(*play, &game.state) {
            Ok(_) => 1,
            Err(_) => 0,
        }
    }).collect()
}

fn main() {
    println!("hnefatafl-rs demo");
    let mut game: SmallBasicGame = Game::new(
        preset::rules::KOCH, 
        preset::boards::BRANDUBH,
    ).expect("Could not create game.");
    loop {
        println!("Board:");
        println!("{}", game.state.board);
        println!("{:?} to play.", game.state.side_to_play);

        let play = get_play();
        match game.do_play(play) {
            Ok(status) => {
                if let Over(outcome) = status {
                    match outcome {
                        Draw(reason) => println!("Game over. Draw {reason:?}."),
                        Win(reason, side) => println!("Game over. Winner is {side:?} ({reason:?})."),
                    }
                    println!("Final board:");
                    println!("{}", game.state.board);
                    return
                }
            },
            Err(e) => println!("Invalid move ({e:?}). Try again.")
        }
    }
}

