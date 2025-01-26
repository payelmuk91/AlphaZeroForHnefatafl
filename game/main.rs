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


fn board_to_matrix<T: BoardState>(game_state: &GameState<T>) -> Vec<Vec<u8>> {
    let side_len = game_state.board.side_len();
    let mut matrix = vec![vec![0; side_len as usize]; side_len as usize];

    // Initialize special tiles
    matrix[0][0] = 20;
    matrix[0][(side_len - 1) as usize] = 20;
    matrix[(side_len - 1) as usize][0] = 20;
    matrix[(side_len - 1) as usize][(side_len - 1) as usize] = 20;
    matrix[(side_len / 2) as usize][(side_len / 2) as usize] = 30;

    // Iterate over the board and add piece values
    for row in 0..side_len {
        for col in 0..side_len {
            let tile = Tile::new(row, col);
            if let Some(piece) = game_state.board.get_piece(tile) {
                let value = match piece.piece_type {
                    PieceType::Soldier => 1,
                    PieceType::Knight => 2,
                    PieceType::King => 5,
                    _ => 0,
                };
                matrix[row as usize][col as usize] += value;
            }
        }
    }

    matrix
}


fn write_to_file(
    file_path: &str,
    matrix: Vec<Vec<u8>>,
    vector: Vec<u8>,
    value1: u8,
    value2: u8,
    max_entries: usize,
) -> std::io::Result<()> {
    let path = Path::new(file_path);
    let mut entries = Vec::new();

    // Read existing entries if the file exists
    if path.exists() {
        let content = read_to_string(file_path)?;
        entries = content.lines().map(|line| line.to_string()).collect();
    }

    // Check if the number of entries exceeds max_entries
    if entries.len() >= max_entries {
        entries.remove(0); // Remove the oldest entry
    }

    // Create a new entry
    let new_entry = format!(
        "{}\n{}\n{}\n{}",
        matrix
            .iter()
            .map(|row| row.iter().map(|&v| v.to_string()).collect::<Vec<_>>().join(","))
            .collect::<Vec<_>>()
            .join("\n"),
        vector.iter().map(|&v| v.to_string()).collect::<Vec<_>>().join(","),
        value1,
        value2
    );

    // Add the new entry to the entries list
    entries.push(new_entry);

    // Write all entries back to the file
    let file = OpenOptions::new().write(true).create(true).truncate(true).open(file_path)?;
    let mut writer = BufWriter::new(file);
    for entry in entries {
        writeln!(writer, "{}", entry)?;
    }

    Ok(())
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

