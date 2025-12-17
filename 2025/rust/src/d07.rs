use std::collections::HashMap;
pub fn p1(lines: Vec<String>) -> String {
    let mut iter = lines.into_iter();
    let first = iter
        .next()
        .unwrap()
        .chars()
        .map(|c| if c == 'S' { '|' } else { c })
        .collect::<String>();
    let final_grid = iter.fold(vec![first], |mut acc, line| {
        let prev = acc.last().unwrap();
        println!("prev: {prev}");
        println!("curr: {line}");
        let ray_indexes = prev
            .chars()
            .enumerate()
            .filter(|(_, c)| *c == '|')
            .map(|x| x.0)
            .collect::<Vec<_>>();
        let splitter_indexes = line
            .chars()
            .enumerate()
            .filter(|(_, c)| *c == '^')
            .map(|x| x.0)
            .collect::<Vec<_>>();
        let used_splitter_indexes = splitter_indexes
            .iter()
            .filter(|splitter_index| ray_indexes.contains(*splitter_index))
            .collect::<Vec<_>>();
        let new_ray_indexes = ray_indexes.iter().fold(Vec::new(), |mut acc, ray_index| {
            let is_split = splitter_indexes.contains(ray_index);
            if is_split {
                acc.push(ray_index - 1);
                acc.push(ray_index + 1);
            } else {
                acc.push(*ray_index);
            }
            acc
        });
        let new_line = (0..line.len())
            .map(|i| {
                if new_ray_indexes.contains(&i) {
                    '|'
                } else if used_splitter_indexes.contains(&&i) {
                    '*'
                } else if splitter_indexes.contains(&i) {
                    '^'
                } else {
                    '.'
                }
            })
            .collect::<String>();
        acc.push(new_line);
        acc
    });
    for s in final_grid.clone() {
        println!("{s}");
    }
    let splits = final_grid
        .into_iter()
        .map(|s| s.chars().filter(|c| *c == '*').collect::<Vec<_>>().len())
        .sum::<usize>();
    format!("{splits}")
}

pub fn p2(lines: Vec<String>) -> String {
    let start_index = lines[0].find('S').unwrap();
    let lookup_table: &mut HashMap<(usize, usize), u64> = &mut (HashMap::new());
    let total = get_timelines(start_index, 0, lines, lookup_table);
    format!("{total}")
}

fn get_timelines(
    x: usize,
    y: usize,
    lines: Vec<String>,
    lookup_table: &mut HashMap<(usize, usize), u64>,
) -> u64 {
    let rest_option = lines.split_first();
    if let Some((_, rest)) = rest_option {
        let target = lines[0].as_str().chars().collect::<Vec<_>>()[x];
        let maybe_found = lookup_table.get(&(x, y));
        if let Some(timelines) = maybe_found {
            *timelines
        } else if target == '^' {
            let timelines = get_timelines(x - 1, y + 1, rest.to_vec(), lookup_table)
                + get_timelines(x + 1, y + 1, rest.to_vec(), lookup_table);
            lookup_table.insert((x, y), timelines);
            timelines
        } else {
            let timelines = get_timelines(x, y + 1, rest.to_vec(), lookup_table);
            lookup_table.insert((x, y), timelines);
            timelines
        }
    } else {
        1
    }
}
