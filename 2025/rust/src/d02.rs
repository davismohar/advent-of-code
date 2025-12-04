use std::ops::RangeInclusive;

pub fn p1(lines: Vec<String>) -> String {
    let line_strs: Vec<&str> = lines.iter().map(|x| x.as_str()).collect();
    let ranges: Vec<RangeInclusive<i64>> = get_ranges(line_strs).unwrap();
    let invalid_ids: Vec<i64> = ranges
        .into_iter()
        .map(get_p1_invalid_ids_from_range)
        .collect::<Result<Vec<Vec<i64>>, std::num::ParseIntError>>()
        .unwrap()
        .into_iter()
        .flatten()
        .collect();

    let sum: i64 = invalid_ids.iter().sum();
    format!("{sum}")
}

pub fn p2(lines: Vec<String>) -> String {
    let line_strs: Vec<&str> = lines.iter().map(|x| x.as_str()).collect();
    let ranges: Vec<RangeInclusive<i64>> = get_ranges(line_strs).unwrap();
    let invalid_ids: Vec<i64> = ranges
        .into_iter()
        .map(get_p2_invalid_ids_from_range)
        .collect::<Result<Vec<Vec<i64>>, std::num::ParseIntError>>()
        .unwrap()
        .into_iter()
        .flatten()
        .collect();

    let sum: i64 = invalid_ids.iter().sum();
    format!("{sum}")
}

fn get_ranges(lines: Vec<&str>) -> Result<Vec<RangeInclusive<i64>>, std::num::ParseIntError> {
    let x: Result<Vec<Vec<RangeInclusive<i64>>>, std::num::ParseIntError> = lines
        .into_iter()
        .filter(|line| !line.is_empty())
        .map(parse_ranges_from_line)
        .collect();
    let y = x?;
    let z = y.into_iter().flatten();
    let yy = z.collect::<Vec<RangeInclusive<i64>>>();
    Ok(yy)
}

fn parse_ranges_from_line(line: &str) -> Result<Vec<RangeInclusive<i64>>, std::num::ParseIntError> {
    line.split(",")
        .map(String::from)
        .filter(|x| !x.is_empty())
        .map(parse_range_from_string)
        .collect()
}

fn parse_range_from_string(
    range_string: String,
) -> Result<RangeInclusive<i64>, std::num::ParseIntError> {
    let parts: Vec<i64> = range_string
        .split("-")
        .map(|int_string| int_string.parse::<i64>())
        .collect::<Result<Vec<i64>, std::num::ParseIntError>>()?;

    Ok(parts[0]..=parts[1])
}

fn get_p1_invalid_ids_from_range(
    range: RangeInclusive<i64>,
) -> Result<Vec<i64>, std::num::ParseIntError> {
    range
        .map(|x| x.to_string())
        .filter(|x| {
            let len = x.len();
            if len % 2 != 0 {
                false
            } else {
                let (front, back) = x.split_at(len / 2);
                front == back
            }
        })
        .map(|x| x.parse::<i64>())
        .collect::<Result<Vec<i64>, std::num::ParseIntError>>()
}

fn get_p2_invalid_ids_from_range(
    range: RangeInclusive<i64>,
) -> Result<Vec<i64>, std::num::ParseIntError> {
    range
        .map(|x| x.to_string())
        .filter(|x| eval_p2_number(x.as_str()))
        .map(|x| x.parse::<i64>())
        .collect::<Result<Vec<i64>, std::num::ParseIntError>>()
}

fn eval_p2_number(num: &str) -> bool {
    let len = num.len();
    let half_len = len / 2;
    let range = 1..=half_len;
    println!("num: {num}");
    for slice_len in range {
        let mut chunks = num.as_bytes().chunks_exact(slice_len);
        // skip if you can't evenly divide the chunks
        if !chunks.remainder().is_empty() {
            continue;
        }
        // we don't ever expect the size to be 0
        let first = chunks.next().unwrap();
        let invalid = chunks.all(|chunk| chunk == first);
        if invalid {
            println!("INVALID NUMBER FOUND: {num}");
            return true;
        } else {
            println!("valid: {num}");
        }
    }
    false
}
