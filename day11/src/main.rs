use itertools::Itertools;
use std::fmt;
use std::io::BufRead;

fn main() {
    let stdin = std::io::stdin();

    let mut map = stdin
        .lock()
        .lines()
        .map(Result::unwrap)
        .fold(Map::new(), |m, line| m.with_line(line));

    println!("{}", map.clone().part_one());
    println!("{}", map.part_two());
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Square {
    Occupied,
    Empty,
    Floor,
}

impl From<u8> for Square {
    fn from(c: u8) -> Square {
        match c {
            b'L' => Square::Empty,
            b'.' => Square::Floor,
            b'#' => Square::Occupied,
            _ => panic!("invalid input"),
        }
    }
}

#[derive(Clone, PartialEq)]
struct Map {
    lines: Vec<Vec<Square>>,
}

impl fmt::Display for Map {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.lines
                .iter()
                .map(|line| {
                    line.iter()
                        .map(|sq| match sq {
                            Square::Empty => 'L',
                            Square::Floor => '.',
                            Square::Occupied => '#',
                        })
                        .collect::<String>()
                })
                .format("\n")
        )
    }
}

impl Map {
    fn new() -> Self {
        Self {
            lines: Vec::with_capacity(100),
        }
    }

    fn with_line(mut self, line: String) -> Self {
        self.lines.push(Vec::with_capacity(100));
        let buf = self.lines.last_mut().unwrap();

        for c in line.bytes() {
            let square = Square::from(c);
            buf.push(square);
        }

        self
    }

    fn part_one(&mut self) -> usize {
        while self.play_one() {}

        self.n_of_occopied()
    }

    fn part_two(&mut self) -> usize {
        while self.play_two() {}

        self.n_of_occopied()
    }

    fn n_of_occopied(&self) -> usize {
        self.lines
            .iter()
            .map(|line| {
                line.iter()
                    .filter(|square| matches!(square, Square::Occupied))
                    .count()
            })
            .sum()
    }

    fn seat(&self, y: i32, x: i32) -> Square {
        if y < 0 || x < 0 {
            return Square::Empty;
        }

        self.lines
            .get(y as usize)
            .and_then(|line| line.get(x as usize).copied())
            .unwrap_or(Square::Empty)
    }

    fn adjecent(&self, y: i32, x: i32) -> [Square; 8] {
        [
            self.seat(y - 1, x - 1),
            self.seat(y - 1, x),
            self.seat(y - 1, x + 1),
            self.seat(y, x - 1),
            self.seat(y, x + 1),
            self.seat(y + 1, x - 1),
            self.seat(y + 1, x),
            self.seat(y + 1, x + 1),
        ]
    }

    fn adjecent_two(&self, y: i32, x: i32) -> [Square; 8] {
        [
            self.first_visible(y - 1, x - 1, |y, x| (y - 1, x - 1)),
            self.first_visible(y - 1, x, |y, x| (y - 1, x)),
            self.first_visible(y - 1, x + 1, |y, x| (y - 1, x + 1)),
            self.first_visible(y, x - 1, |y, x| (y, x - 1)),
            self.first_visible(y, x + 1, |y, x| (y, x + 1)),
            self.first_visible(y + 1, x - 1, |y, x| (y + 1, x - 1)),
            self.first_visible(y + 1, x, |y, x| (y + 1, x)),
            self.first_visible(y + 1, x + 1, |y, x| (y + 1, x + 1)),
        ]
    }

    fn first_visible(&self, y: i32, x: i32, f: fn(i32, i32) -> (i32, i32)) -> Square {
        let seat = self.seat(y, x);

        if seat == Square::Floor {
            let (ny, nx) = f(y, x);
            self.first_visible(ny, nx, f)
        } else {
            seat
        }
    }

    fn play_one(&mut self) -> bool {
        self.play(|s, i, j| s.adjecent(i, j), 4)
    }

    fn play_two(&mut self) -> bool {
        self.play(|s, i, j| s.adjecent_two(i, j), 5)
    }

    fn play(&mut self, f: impl Fn(&Self, i32, i32) -> [Square; 8], greed: usize) -> bool {
        let old = self.clone();

        for i in 0..self.lines.len() {
            for j in 0..self.lines[i].len() {
                let (i, j) = (i as i32, j as i32);
                let adjecent = f(&old, i, j);

                let current = &mut self.lines[i as usize][j as usize];

                match old.seat(i, j) {
                    Square::Empty if adjecent.iter().all(|&sq| sq != Square::Occupied) => {
                        *current = Square::Occupied;
                    }
                    Square::Occupied
                        if adjecent
                            .iter()
                            .filter(|&&sq| sq == Square::Occupied)
                            .count()
                            >= greed =>
                    {
                        *current = Square::Empty;
                    }
                    _ => {}
                };
            }
        }

        old != *self
    }
}
