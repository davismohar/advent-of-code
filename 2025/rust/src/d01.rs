pub fn p1(lines: Vec<String>) -> String {
    let nums = &parse_lines(lines).unwrap();
    let mut pos = 50;
    let mut count = 0;
    for num in nums {
        let new_pos = (num + pos) % 100;
        if new_pos == 0 {
            count += 1;
        }
        pos = new_pos
    }
    format!("{count}")
}
pub fn p2(lines: Vec<String>) -> String {
    // have a sense this is stupid, but trial and error got me here
    let nums = &parse_lines(lines).unwrap();
    let mut pos = 50;
    let mut rotations = 0;
    for num in nums {
        let sum = num + pos;
        let new_pos = if sum >= 0 {
            (num + pos) % 100
        } else {
            ((num + pos) % 100) + 100
        };
        let new_rotations = if sum > 0 {
            sum / 100
        } else if pos == 0 {
            sum.abs() / 100
        } else {
            (sum.abs() / 100) + 1
        };
        rotations += new_rotations;
        if new_pos == 100 {
            pos = 0
        } else {
            pos = new_pos
        }
    }
    format!("{rotations}")
}

fn parse_lines(lines: Vec<String>) -> Result<Vec<i32>, std::num::ParseIntError> {
    lines
        .into_iter()
        .filter(|line| !line.is_empty())
        .map(parse_line)
        .collect()
}

fn parse_line(line: String) -> Result<i32, std::num::ParseIntError> {
    let num = line[1..].parse::<i32>()?;
    match line {
        i if i.starts_with('L') => Ok(-num),
        _ => Ok(num),
    }
}
