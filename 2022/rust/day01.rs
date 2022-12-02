use std::collections::BinaryHeap;
use std::fs;
use std::path;

pub fn read_input(file: &path::Path) -> BinaryHeap<u32> {
    let content = fs::read_to_string(file).expect("Should have been able to read file");
    let mut current_calories = 0;
    let mut heap = BinaryHeap::new();
    for line in content.lines() {
        match line.parse::<u32>() {
            Ok(calories) => current_calories += calories,
            Err(_) => {
                heap.push(current_calories);
                current_calories = 0;
            }
        }
    }
    heap.push(current_calories);

    heap
}

pub fn problem1(file: &path::Path) -> u32 {
    let mut heap = read_input(file);
    heap.pop().unwrap()
}

pub fn problem2(file: &path::Path) -> u32 {
    let mut heap = read_input(file);
    heap.pop().unwrap() + heap.pop().unwrap() + heap.pop().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn problem1_sample_input() {
        let result = problem1(path::Path::new("problems/day01/input_sample.txt"));
        assert_eq!(result, 24000);
    }

    #[test]
    fn problem1_real_input() {
        let result = problem1(path::Path::new("problems/day01/input.txt"));
        assert_eq!(result, 67450);
    }

    #[test]
    fn problem2_sample_input() {
        let result = problem2(path::Path::new("problems/day01/input_sample.txt"));
        assert_eq!(result, 45000);
    }

    #[test]
    fn problem2_real_input() {
        let result = problem2(path::Path::new("problems/day01/input.txt"));
        assert_eq!(result, 199357);
    }
}
