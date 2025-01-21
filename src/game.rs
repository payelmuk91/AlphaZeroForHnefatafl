use std::io::Write;


// Creating the board and pieces for the game


pub struct Tafl {
    size: usize,
    board: Vec<Vec<i32>>,
    pieces: Vec<Vec<i32>>,
}

impl Tafl {
    pub fn new(size: usize) -> Self {
        Tafl {
            size,
            board: vec![vec![0; size]; size],
            pieces: Vec::new(),
        }
    }
}

pub struct Hnefatafl {
    tafl: Tafl,
}

impl Hnefatafl {
    pub fn new() -> Self {
        let tafl = Tafl::new(7);
        
        // 2 for corner, 1 for everything else
        let mut board = vec![vec![1; 7]; 7];
        let corners = vec![(0, 0), (0, 6), (6, 0), (6, 6)];
        for &(x, y) in &corners {
            board[x][y] = 2;
        }
        
        // piece position: [x,y,piece type] 1 for defender, -1 for attacker, 2 for king
        let pieces = vec![
            [3, 0, -1], [3, 1, -1], 
            [0, 3, -1], [1, 3, -1],
            [3, 5, -1], [3, 6, -1],
            [5, 3, -1], [6, 3, -1],

            [2, 3, 1], [3, 2, 1], [3, 4, 1], [4, 3, 1],
            [3,3,2]
        ];
        Hnefatafl { tafl: Tafl { size: 7, board, pieces } }
    }
}

// Game logic and rules

pub struct Board {
    size: usize,
    width: usize,
    height: usize,
    board: Vec<Vec<i32>>,
    pieces: Vec<Vec<i32>>,
    time: i32,
    done: i32,
}

impl Board {
    pub fn new(tafl: Tafl) -> Self {
        Board {
            size: tafl.size,
            width: tafl.size,
            height: tafl.size,
            board: tafl.board,
            pieces: tafl.pieces,
            time: 0,
            done: 0,
        }
    }

    pub fn get_player_to_move(&self) -> i32 {
        -(self.time % 2 * 2 - 1)
    }

    pub fn get_image(&self) -> Vec<Vec<i32>> {
        let mut image = vec![vec![0; self.width]; self.height];
        for item in &self.board {
            image[item[1] as usize][item[0] as usize] = item[2] * 10;
        }
        for piece in &self.pieces {
            if piece[0] >= 0 {
                image[piece[1] as usize][piece[0] as usize] += piece[2];
            }
        }
        image
    }

    pub fn count_diff(&self, color: i32) -> i32 {
        let mut count = 0;
        for p in &self.pieces {
            if p[0] >= 0 {
                if p[2] * color > 0 {
                    count += 1;
                } else {
                    count -= 1;
                }
            }
        }
        count
    }

    pub fn get_legal_moves(&self, color: i32) -> Vec<(i32, i32, i32, i32)> {
        self.get_valid_moves(color)
    }

    pub fn has_legal_moves(&self, color: i32) -> bool {
        !self.get_valid_moves(color).is_empty()
    }

    pub fn execute_move(&mut self, mv: (i32, i32, i32, i32), color: i32) {
        let (x1, y1, x2, y2) = mv;
        let pieceno = self.get_piece_no(x1, y1);
        if self.is_legal_move(pieceno, x2, y2) >= 0 {
            self.move_by_piece_no(pieceno, x2, y2);
        }
    }

    fn is_legal_move(&self, pieceno: usize, x2: i32, y2: i32) -> i32 {
        if x2 < 0 || y2 < 0 || x2 >= self.width as i32 || y2 >= self.height as i32 {
            return -1;
        }
        let piece = &self.pieces[pieceno];
        let (x1, y1) = (piece[0], piece[1]);
        if x1 < 0 {
            return -2;
        }
        if x1 != x2 && y1 != y2 {
            return -3;
        }
        if x1 == x2 && y1 == y2 {
            return -4;
        }
        let piecetype = piece[2];
        if (piecetype == -1 && self.time % 2 == 0) || (piecetype != -1 && self.time % 2 == 1) {
            return -5;
        }
        for item in &self.board {
            if item[0] == x2 && item[1] == y2 && item[2] > 0 {
                if piecetype != 2 {
                    return -10;
                }
            }
        }
        for apiece in &self.pieces {
            if y1 == y2 && y1 == apiece[1] && ((x1 < apiece[0] && x2 >= apiece[0]) || (x1 > apiece[0] && x2 <= apiece[0])) {
                return -20;
            }
            if x1 == x2 && x1 == apiece[0] && ((y1 < apiece[1] && y2 >= apiece[1]) || (y1 > apiece[1] && y2 <= apiece[1])) {
                return -20;
            }
        }
        0
    }

    fn get_captures(&self, pieceno: usize, x2: i32, y2: i32) -> Vec<Vec<i32>> {
        let mut captures = Vec::new();
        let piece = &self.pieces[pieceno];
        let piecetype = piece[2];
        for apiece in &self.pieces {
            if piecetype * apiece[2] < 0 {
                let d1 = apiece[0] - x2;
                let d2 = apiece[1] - y2;
                if (d1.abs() == 1 && d2 == 0) || (d2.abs() == 1 && d1 == 0) {
                    for bpiece in &self.pieces {
                        if piecetype * bpiece[2] > 0 && !(piece[0] == bpiece[0] && piece[1] == bpiece[1]) {
                            let e1 = bpiece[0] - apiece[0];
                            let e2 = bpiece[1] - apiece[1];
                            if d1 == e1 && d2 == e2 {
                                captures.push(apiece.clone());
                            }
                        }
                    }
                }
            }
        }
        captures
    }

    fn move_by_piece_no(&mut self, pieceno: usize, x2: i32, y2: i32) -> i32 {
        if self.is_legal_move(pieceno, x2, y2) != 0 {
            return self.is_legal_move(pieceno, x2, y2);
        }
        self.time += 1;
        let piece = &mut self.pieces[pieceno];
        piece[0] = x2;
        piece[1] = y2;
        let captures = self.get_captures(pieceno, x2, y2);
        for c in captures {
            self.pieces.iter_mut().find(|p| p == &&c).unwrap()[0] = -99;
        }
        self.done = self.get_win_lose();
        captures.len() as i32
    }

    fn get_win_lose(&self) -> i32 {
        if self.time > 50 {
            return -1;
        }
        for apiece in &self.pieces {
            if apiece[2] == 2 && apiece[0] > -1 {
                for item in &self.board {
                    if item[0] == apiece[0] && item[1] == apiece[1] && item[2] == 1 {
                        return 1;
                    }
                }
                return 0;
            }
        }
        -1
    }

    fn get_piece_no(&self, x: i32, y: i32) -> usize {
        self.pieces.iter().position(|p| p[0] == x && p[1] == y).unwrap_or(usize::MAX)
    }

    fn get_valid_moves(&self, player: i32) -> Vec<(i32, i32, i32, i32)> {
        let mut moves = Vec::new();
        for (pieceno, piece) in self.pieces.iter().enumerate() {
            if piece[2] * player > 0 {
                for x in 0..self.width as i32 {
                    if self.is_legal_move(pieceno, x, piece[1]) >= 0 {
                        moves.push((piece[0], piece[1], x, piece[1]));
                    }
                }
                for y in 0..self.height as i32 {
                    if self.is_legal_move(pieceno, piece[0], y) >= 0 {
                        moves.push((piece[0], piece[1], piece[0], y));
                    }
                }
            }
        }
        moves
    }
}


// Create Game struct to hold the game state

pub struct TaflGame {
    name: String,
}

impl TaflGame {
    pub fn new(name: &str) -> Self {
        TaflGame {
            name: name.to_string(),
        }
    }

    pub fn get_init_board(&self) -> Board {
        match self.name.as_str() {
            "Brandubh" => Board::new(Tafl::new(7)), // Assuming Brandubh is 7x7
            "ArdRi" => Board::new(Tafl::new(7)), // Assuming ArdRi is 7x7
            "Tablut" => Board::new(Tafl::new(9)), // Assuming Tablut is 9x9
            "Tawlbwrdd" => Board::new(Tafl::new(11)), // Assuming Tawlbwrdd is 11x11
            "Hnefatafl" => Board::new(Hnefatafl::new().tafl),
            "AleaEvangelii" => Board::new(Tafl::new(19)), // Assuming Alea Evangelii is 19x19
            _ => panic!("Unknown game name"),
        }
    }

    pub fn get_board_size(&self) -> (usize, usize) {
        let board = self.get_init_board();
        (board.size, board.size)
    }

    pub fn get_action_size(&self) -> usize {
        let board = self.get_init_board();
        board.size.pow(4)
    }

    pub fn get_next_state(&self, board: &mut Board, player: i32, action: (i32, i32, i32, i32)) -> (Board, i32) {
        let mut new_board = board.clone();
        new_board.execute_move(action, player);
        (new_board, -player)
    }

    pub fn get_valid_moves(&self, board: &Board, player: i32) -> Vec<bool> {
        let mut valids = vec![false; self.get_action_size()];
        let legal_moves = board.get_legal_moves(board.get_player_to_move());
        if legal_moves.is_empty() {
            valids[valids.len() - 1] = true;
            return valids;
        }
        for (x1, y1, x2, y2) in legal_moves {
            let index = (x1 + y1 * board.size as i32 + x2 * board.size.pow(2) as i32 + y2 * board.size.pow(3) as i32) as usize;
            valids[index] = true;
        }
        valids
    }

    pub fn get_game_ended(&self, board: &Board, player: i32) -> i32 {
        board.done * player
    }

    pub fn get_canonical_form(&self, board: &Board, _player: i32) -> Board {
        board.clone()
    }

    pub fn get_symmetries(&self, board: &Board, pi: Vec<f32>) -> Vec<(Board, Vec<f32>)> {
        vec![(board.clone(), pi)]
    }

    pub fn string_representation(&self, board: &Board) -> String {
        format!("{:?}", board)
    }

    pub fn get_score(&self, board: &Board, player: i32) -> i32 {
        if board.done != 0 {
            return 1000 * board.done * player;
        }
        board.count_diff(player)
    }
}

pub fn display(board: &Board) {
    let render_chars = [
        ("-1", "b"),
        ("0", " "),
        ("1", "W"),
        ("2", "K"),
        ("10", "#"),
        ("12", "E"),
        ("20", "_"),
        ("22", "x"),
    ].iter().cloned().collect::<std::collections::HashMap<_, _>>();

    println!("---------------------");
    let image = board.get_image();

    print!("  ");
    for i in 0..image.len() {
        print!("{} ", i);
    }
    println!();

    for i in (0..image.len()).rev() {
        print!("{:2} ", i);
        for &col in &image[i] {
            let c = render_chars.get(&col.to_string()).unwrap_or(&" ");
            print!("{}", c);
        }
        println!();
    }
    println!("---------------------");
}
