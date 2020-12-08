use std::collections::HashSet;
use std::io::BufRead;

fn main() {
    let stdin = std::io::stdin();
    let mut vm = VM::<HashSet<usize>>::new(
        stdin
            .lock()
            .lines()
            .map(Result::unwrap)
            .map(Instr::from)
            .collect(),
        HashSet::new(),
    );

    println!("part 1: {}", vm.run(0).1);

    for edit in 0..vm.instrs.len() {
        vm.reset();

        let instr = vm.instrs[edit];
        match instr {
            Instr::Acc(_) => continue,
            Instr::Jmp(n) => vm.instrs[edit] = Instr::Nop(n),
            Instr::Nop(n) => vm.instrs[edit] = Instr::Jmp(n),
        }

        if let (ExitCode::Normal, n) = vm.run(0) {
            println!("part 2: {}", n);
            return;
        }

        vm.instrs[edit] = instr;
    }
}

#[derive(Clone, Copy)]
enum Instr {
    Jmp(isize),
    Acc(isize),
    Nop(isize),
}

impl<I: AsRef<str>> From<I> for Instr {
    fn from(s: I) -> Instr {
        let s = s.as_ref();
        let mut iter = s.split(|c| c == ' ');

        let op = iter.next().unwrap();
        let n = {
            let s = iter.next().unwrap();
            let op = s.as_bytes()[0];
            let n = s[1..].parse::<isize>().unwrap();
            match op {
                b'+' => n,
                b'-' => -n,
                _ => unreachable!(),
            }
        };

        match op {
            "nop" => Instr::Nop(n),
            "acc" => Instr::Acc(n),
            "jmp" => Instr::Jmp(n),
            _ => unreachable!(),
        }
    }
}

struct VM<S> {
    instrs: Vec<Instr>,
    acc: isize,

    state: S,
}

trait State {
    fn update_and_signal(&mut self, iptr: usize, instr: Instr) -> bool;
}

impl State for HashSet<usize> {
    fn update_and_signal(&mut self, iptr: usize, _: Instr) -> bool {
        !self.insert(iptr)
    }
}

enum ExitCode {
    Normal,
    Signal,
}

impl<S: State> VM<S> {
    fn new(instrs: Vec<Instr>, state: S) -> VM<S> {
        VM {
            instrs,
            acc: 0,
            state,
        }
    }

    fn run(&mut self, iptr: usize) -> (ExitCode, isize) {
        if iptr == self.instrs.len() {
            return (ExitCode::Normal, self.acc);
        }

        let instr = self.instrs[iptr];
        if self.state.update_and_signal(iptr, instr) {
            return (ExitCode::Signal, self.acc);
        }

        match instr {
            Instr::Nop(_) => self.run(iptr + 1),
            Instr::Acc(n) => {
                self.acc += n;
                self.run(iptr + 1)
            }
            Instr::Jmp(offs) => self.run((iptr as isize + offs) as usize),
        }
    }
}

impl VM<HashSet<usize>> {
    fn reset(&mut self) {
        self.acc = 0;
        self.state.clear();
    }
}
