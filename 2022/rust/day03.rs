use std::collections::HashSet;
use std::fs;
use std::path;

type Rucksack = Vec<char>;

fn read_input(file: &path::Path) -> Vec<Rucksack> {
    let content = fs::read_to_string(file).expect("Should have been able to read file");
    let mut rucksacks = vec![];
    for line in content.lines() {
        rucksacks.push(line.chars().collect());
    }
    rucksacks
}

fn score(ch: char) -> u32 {
    if ch.is_uppercase() {
        ch as u32 - 38
    } else {
        ch as u32 - 96
    }
}

pub fn problem1(file: &path::Path) -> u32 {
    let rucksacks = read_input(file);
    let mut output = 0;
    for rucksack in rucksacks {
        let mid = rucksack.len() / 2;
        let compartment1: HashSet<&char> = HashSet::from_iter(rucksack[..mid].iter());
        let compartment2: HashSet<&char> = HashSet::from_iter(rucksack[mid..].iter());
        output += score(**compartment1.intersection(&compartment2).next().unwrap());
    }
    output
}

pub fn problem2(file: &path::Path) -> u32 {
    let input = read_input(file);
    let input_as_sets = input
        .iter()
        .map(|rucksack| -> HashSet<&char> { HashSet::from_iter(rucksack) });
    let mut rucksacks = input_as_sets.peekable();
    let mut output = 0;
    while let Some(_) = rucksacks.peek() {
        let r1 = rucksacks.next().unwrap();
        let r2 = rucksacks.next().unwrap();
        let r3 = rucksacks.next().unwrap();
        output += score(
            **r1.intersection(&r2)
                .map(|ch| *ch)
                .collect::<HashSet<&char>>()
                .intersection(&r3)
                .next()
                .unwrap(),
        );
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn problem1_sample_input() {
        let result = problem1(path::Path::new("problems/day03/input_sample.txt"));
        assert_eq!(result, 157);
    }

    #[test]
    fn problem1_real_input() {
        let result = problem1(path::Path::new("problems/day03/input.txt"));
        assert_eq!(result, 7863);
    }

    #[test]
    fn problem2_sample_input() {
        let result = problem2(path::Path::new("problems/day03/input_sample.txt"));
        assert_eq!(result, 70);
    }

    #[test]
    fn problem2_real_input() {
        let result = problem2(path::Path::new("problems/day03/input.txt"));
        assert_eq!(result, 2488);
    }
}
