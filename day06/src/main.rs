use std::collections::{HashMap, HashSet};
use std::io::Read;

fn main() {
    let mut stdin = std::io::stdin();
    let mut input = String::with_capacity(8000);
    stdin
        .read_to_string(&mut input)
        .expect("could not read input");

    if cfg!(feature = "part1") {
        println!("{}", part1(input));
    } else if cfg!(feature = "part2") {
        println!("{}", part2(input));
    }
}

fn part1(input: String) -> usize {
    input.split("\n\n").fold(0, |acc, group| {
        group
            .bytes()
            .fold((acc, HashSet::new()), |(acc, mut used), b| {
                if b != b'\n' && used.insert(b) {
                    (acc + 1, used)
                } else {
                    (acc, used)
                }
            })
            .0
    })
}

fn part2(input: String) -> usize {
    let input = input.trim_end_matches(|c| c == '\n');

    input.split("\n\n").fold(0, |acc, group| {
        let mut responses = HashMap::new();
        let mut members = 1;

        for b in group.as_bytes() {
            if *b == b'\n' {
                members += 1;
                continue;
            }
            let n = responses.entry(b).or_insert(0);
            *n += 1;
        }

        acc + responses.values().filter(|&&v| v == members).count()
    })
}
