use std::ops::Range;

pub fn p1(lines: Vec<String>) -> String {
    let total_len = lines[0].len();
    let height = lines.len();
    println!("total_len: {total_len}");
    let grid = lines.iter().map(|it| it.chars()).collect::<Vec<_>>();
    let grid_vec = grid
        .into_iter()
        .map(|x| x.collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let breaks = grid_vec[height - 1]
        .clone()
        .into_iter()
        .enumerate()
        .filter(|(_, c)| *c == '*' || *c == '+')
        .map(|it| it.0)
        .collect::<Vec<_>>();
    for b in breaks.clone() {
        println!("{b}")
    }
    let ranges = build_equation_ranges(breaks, total_len);
    for range in ranges.clone() {
        let start = range.start;
        let end = range.end;
        println!("{start}..{end}");
    }
    let results = ranges
        .into_iter()
        .map(|range| {
            let nums = &grid_vec[0..height - 1]
                .iter()
                .map(|chars| {
                    chars[range.clone()]
                        .iter()
                        .filter(|char| **char != ' ')
                        .collect::<String>()
                        .parse::<i64>()
                        .unwrap()
                })
                .collect::<Vec<_>>();
            let op = &grid_vec[height - 1][range.clone()]
                .iter()
                .filter(|char| **char != ' ')
                .collect::<String>();
            for num in nums.clone() {
                print!("{num} ")
            }
            println!("{op}");
            match (*op).as_str() {
                "*" => nums.iter().product::<i64>(),
                "+" => nums.iter().sum::<i64>(),
                _ => panic!("not plus or mult"),
            }
        })
        .collect::<Vec<_>>();
    let sum: i64 = results.into_iter().sum();
    format!("{sum}")
}

pub fn p2(lines: Vec<String>) -> String {
    let total_len = lines[0].len();
    let height = lines.len();
    println!("total_len: {total_len}");
    let grid = lines.iter().map(|it| it.chars()).collect::<Vec<_>>();
    let grid_vec = grid
        .into_iter()
        .map(|x| x.collect::<Vec<_>>())
        .collect::<Vec<_>>();
    for x in grid_vec.clone() {
        let s = x.iter().collect::<String>();
        println!("{s}")
    }
    let breaks = grid_vec[height - 1]
        .clone()
        .into_iter()
        .enumerate()
        .filter(|(_, c)| *c == '*' || *c == '+')
        .map(|it| it.0)
        .collect::<Vec<_>>();
    for b in breaks.clone() {
        println!("{b}")
    }
    let ranges = build_equation_ranges(breaks, total_len);
    for range in ranges.clone() {
        let start = range.start;
        let end = range.end;
        println!("{start}..{end}");
    }
    let results = ranges
        .into_iter()
        .map(|range| {
            let nums = (range.start..range.end)
                .map(|x| {
                    println!("x: {x}");
                    (0..height - 1)
                        .fold(Vec::new(), |mut acc, y| {
                            let char = grid_vec[y][x];
                            println!("({x},{y}) char: `{char}`");
                            acc.push(char);
                            acc
                        })
                        .iter()
                        .filter(|i| **i != ' ')
                        .collect::<String>()
                })
                .map(|x| {
                    println!("acc `{x}`");
                    x.parse::<i64>().unwrap()
                })
                .collect::<Vec<_>>();

            let op = &grid_vec[height - 1][range.clone()]
                .iter()
                .filter(|char| **char != ' ')
                .collect::<String>();
            let res = match (*op).as_str() {
                "*" => nums.iter().product::<i64>(),
                "+" => nums.iter().sum::<i64>(),
                _ => panic!("not plus or mult"),
            };
            print!("eq: ");
            for num in nums.clone() {
                print!("{num} ")
            }
            println!("{op} = {res}");
            res
        })
        .collect::<Vec<_>>();
    let sum: i64 = results.into_iter().sum();
    format!("{sum}")
}

pub fn build_equation_ranges(breaks: Vec<usize>, total_len: usize) -> Vec<Range<usize>> {
    let mut ranges = Vec::new();
    let mut iter = breaks.into_iter().peekable();
    while let Some(item) = iter.next() {
        let default = total_len + 1;
        let next = iter.peek().unwrap_or(&default);
        ranges.push(item..(*next - 1))
    }
    ranges
}
