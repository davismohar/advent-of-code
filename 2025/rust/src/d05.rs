pub fn p1(lines: Vec<String>) -> String {
    let mut line_iter = lines.iter();
    let ranges = line_iter
        .by_ref()
        .take_while(|x| !x.is_empty())
        .map(|x| parse_range(x.clone()))
        .collect::<Vec<_>>();
    let ids = line_iter
        .filter(|x| !x.is_empty())
        .map(|x| x.parse::<i64>())
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let fresh_ids = ids
        .iter()
        .filter(|id| is_fresh(**id, ranges.as_slice()))
        .collect::<Vec<_>>();
    let count = fresh_ids.len();
    format!("{count}")
}

pub fn p2(lines: Vec<String>) -> String {
    let mut ranges = lines
        .iter()
        .take_while(|x| !x.is_empty())
        .map(|x| parse_range(x.clone()))
        .collect::<Vec<_>>();
    ranges.sort_by(|(l1, _), (l2, _)| l1.cmp(l2));
    let collapsed_ranges =
        ranges
            .iter()
            .fold(Vec::new(), |mut acc: Vec<(i64, i64)>, (lower, upper)| {
                let overlapping_vec = acc.iter().filter(|(_, u)| u >= lower).collect::<Vec<_>>();
                let overlapping = overlapping_vec.iter().max_by(|(_, u1), (_, u2)| u1.cmp(u2));
                match overlapping {
                    Some((_, u)) => {
                        if u <= upper {
                            let new_lower = u + 1;
                            let new_pair = (new_lower, *upper);
                            acc.push(new_pair)
                        }
                    }
                    None => acc.push((*lower, *upper)),
                };
                acc
            });
    let total: i64 = collapsed_ranges.into_iter().map(|(l, u)| u - l + 1).sum();
    format!("{total}")
}

fn parse_range(line: String) -> (i64, i64) {
    let parts = line
        .split('-')
        .map(|x| x.parse::<i64>())
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    (parts[0], parts[1])
}

fn is_fresh(id: i64, fresh_ranges: &[(i64, i64)]) -> bool {
    fresh_ranges
        .iter()
        .any(|(lower, upper)| *lower <= id && id <= *upper)
}
