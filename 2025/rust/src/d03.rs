use std::num::ParseIntError;

pub fn p1(lines: Vec<String>) -> String {
    let sums: Result<Vec<i64>, ParseIntError> =
        lines.into_iter().map(p1_get_largest_joltage).collect();

    let sum: i64 = sums.unwrap().into_iter().sum();
    format!("{sum}")
}

pub fn p2(lines: Vec<String>) -> String {
    let sums: Result<Vec<i64>, ParseIntError> =
        lines.into_iter().map(p2_get_largest_joltage).collect();

    let sum: i64 = sums.unwrap().into_iter().sum();
    format!("{sum}")
}

fn get_ints_from_line(line: String) -> Result<Vec<i64>, ParseIntError> {
    line.chars().map(|x| x.to_string().parse::<i64>()).collect()
}

fn p1_get_largest_joltage(line: String) -> Result<i64, ParseIntError> {
    let ints = get_ints_from_line(line)?;
    // some unwraps because we assume the list is at least 2
    let (_, ints_minus_last) = ints.split_last().unwrap();
    let first = ints_minus_last.iter().max().unwrap();
    let first_pos = ints_minus_last.iter().position(|x| x == first).unwrap();
    let (_, rest) = ints.split_at(first_pos + 1);
    let second = rest.iter().max().unwrap();
    let joltage = format!("{first}{second}").parse::<i64>()?;
    Ok(joltage)
}

fn p2_get_largest_joltage(line: String) -> Result<i64, ParseIntError> {
    println!("{line}");
    let ints = get_ints_from_line(line)?;
    let mut largest_ints: Vec<i64> = Vec::new();
    let mut left_boundary = 0;
    for i in 0..12 {
        let len = ints.len();
        let right_boundary = len - 11 + i;
        let working = &ints[left_boundary..right_boundary];
        for i in working {
            print!("{i}");
        }
        println!("\nleft_boundary: {left_boundary}, right_boundary: {right_boundary}");
        let max_in_working = working.iter().max().unwrap();
        let max_index = working.iter().position(|x| x == max_in_working).unwrap();
        let x = max_in_working.to_owned();
        println!("{x}");
        largest_ints.push(x);
        left_boundary += max_index + 1;
    }
    let joltage_string = largest_ints
        .iter()
        .fold(String::new(), |acc, x| format!("{acc}{x}"));

    println!("joltage: {joltage_string}");
    joltage_string.parse::<i64>()
}
