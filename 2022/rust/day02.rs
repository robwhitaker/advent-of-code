use crate::utils::throw;
use std::fs;
use std::path;

struct Round {
    opponent: Move,
    player: Move,
    outcome: Outcome,
}

#[derive(PartialEq)]
enum Move {
    Rock,
    Paper,
    Scissors,
}

enum Outcome {
    Win,
    Lose,
    Draw,
}

fn char_to_move(ch: &str) -> Move {
    match ch {
        "A" => Move::Rock,
        "B" => Move::Paper,
        "C" => Move::Scissors,
        "X" => Move::Rock,
        "Y" => Move::Paper,
        "Z" => Move::Scissors,
        _ => throw("bad input"),
    }
}

fn char_to_outcome(ch: &str) -> Outcome {
    match ch {
        "X" => Outcome::Lose,
        "Y" => Outcome::Draw,
        "Z" => Outcome::Win,
        _ => throw("bad input"),
    }
}

fn read_input(file: &path::Path) -> Vec<Round> {
    let contents = fs::read_to_string(file).expect("should be able to read file");
    let mut rounds = Vec::new();
    for line in contents.lines() {
        let split: Vec<&str> = line.split(" ").collect();
        if let &[opponent, player] = &split[..2] {
            rounds.push(Round {
                opponent: char_to_move(opponent),
                player: char_to_move(player),
                outcome: char_to_outcome(player),
            })
        } else {
            throw("bad input")
        }
    }
    rounds
}

fn score_outcome(outcome: &Outcome) -> u32 {
    match outcome {
        Outcome::Win => 6,
        Outcome::Draw => 3,
        Outcome::Lose => 0,
    }
}

fn score_move(m: &Move) -> u32 {
    match m {
        Move::Rock => 1,
        Move::Paper => 2,
        Move::Scissors => 3,
    }
}

pub fn problem1(file: &path::Path) -> u32 {
    let rounds = read_input(file);
    let mut result = 0;
    for round in rounds {
        let Round {
            opponent, player, ..
        } = round;
        result += score_move(&player);
        match (&opponent, &player) {
            (Move::Rock, Move::Paper) => result += score_outcome(&Outcome::Win),
            (Move::Paper, Move::Scissors) => result += score_outcome(&Outcome::Win),
            (Move::Scissors, Move::Rock) => result += score_outcome(&Outcome::Win),
            _ => {
                if opponent == player {
                    result += score_outcome(&Outcome::Draw);
                }
            }
        }
    }
    result
}

pub fn problem2(file: &path::Path) -> u32 {
    let rounds = read_input(file);
    let mut result = 0;
    for round in rounds {
        let Round {
            opponent, outcome, ..
        } = round;
        result += score_outcome(&outcome);
        match (&opponent, &outcome) {
            (Move::Rock, Outcome::Win) => result += score_move(&Move::Paper),
            (Move::Paper, Outcome::Win) => result += score_move(&Move::Scissors),
            (Move::Scissors, Outcome::Win) => result += score_move(&Move::Rock),
            (Move::Rock, Outcome::Lose) => result += score_move(&Move::Scissors),
            (Move::Paper, Outcome::Lose) => result += score_move(&Move::Rock),
            (Move::Scissors, Outcome::Lose) => result += score_move(&Move::Paper),
            (_, Outcome::Draw) => result += score_move(&opponent),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn problem1_sample_input() {
        let result = problem1(path::Path::new("problems/day02/input_sample.txt"));
        assert_eq!(result, 15);
    }

    #[test]
    fn problem1_real_input() {
        let result = problem1(path::Path::new("problems/day02/input.txt"));
        assert_eq!(result, 11150);
    }

    #[test]
    fn problem2_sample_input() {
        let result = problem2(path::Path::new("problems/day02/input_sample.txt"));
        assert_eq!(result, 12);
    }

    #[test]
    fn problem2_real_input() {
        let result = problem2(path::Path::new("problems/day02/input.txt"));
        assert_eq!(result, 8295);
    }
}
