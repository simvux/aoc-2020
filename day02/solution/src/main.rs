use std::io::BufRead;
use std::ops::Range;

fn main() {
    if cfg!(feature = "part1") {
        part_1();
    } else if cfg!(feature = "part2") {
        part_2();
    }
}

fn part_1() {
    let answer = check_on_stdin(|line| {
        let (policy, password) = unsafe { parse(line) };
        policy.is_valid(password)
    });

    println!("part 1: {}", answer);
}

fn part_2() {
    let answer = check_on_stdin(|line| unsafe {
        let (policy, password) = parse(line);
        policy.is_valid_two(password)
    });

    println!("part 2: {}", answer);
}

fn check_on_stdin<F: Fn(&[u8]) -> bool>(f: F) -> usize {
    let stdin = std::io::stdin();
    stdin.lock().split(b'\n').fold(0, |ok, line| {
        let line = line.unwrap();
        match f(&line) {
            true => ok + 1,
            false => ok,
        }
    })
}

unsafe fn parse(line: &[u8]) -> (Policy, &[u8]) {
    let mut segments = line.split(|b| b"- :".contains(b));

    let from = unwrap_unchecked_opt(segments.next());
    let to = unwrap_unchecked_opt(segments.next());
    let letter = unwrap_unchecked_opt(segments.next());
    let whitespace = unwrap_unchecked_opt(segments.next());

    let password = &line[from.len() + to.len() + letter.len() + whitespace.len() + 4..];
    // let password = unwrap_unchecked_opt(segments.next());

    (
        Policy {
            c: letter[0],
            amount: unwrap_unchecked(std::str::from_utf8_unchecked(from).parse())
                ..unwrap_unchecked(std::str::from_utf8_unchecked(to).parse::<u16>()) + 1,
        },
        password,
    )
}

fn unwrap_unchecked<T, E>(v: Result<T, E>) -> T {
    match v {
        Ok(v) => v,
        Err(_) => unsafe { std::hint::unreachable_unchecked() },
    }
}

fn unwrap_unchecked_opt<T>(v: Option<T>) -> T {
    match v {
        Some(v) => v,
        None => unsafe { std::hint::unreachable_unchecked() },
    }
}

struct Policy {
    c: u8,
    amount: Range<u16>,
}

impl Policy {
    fn is_valid(self, pass: &[u8]) -> bool {
        let occourance = pass
            .iter()
            .fold(0 as u16, |acc, &b| if b == self.c { acc + 1 } else { acc });
        self.amount.contains(&occourance)
    }

    unsafe fn is_valid_two(self, pass: &[u8]) -> bool {
        let a = self.amount.start - 1;
        let b = self.amount.end - 2;

        let a_yes = *pass.get_unchecked(a as usize) == self.c;
        let b_yes = *pass.get_unchecked(b as usize) == self.c;

        (a_yes && !b_yes) || (b_yes && !a_yes)
    }
}

