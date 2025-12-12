use std::option::Option;

pub fn p1(lines: Vec<String>) -> String {
    let grid = lines
        .iter()
        .map(|x| x.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let accessable_forklift_locations = get_accessable_forklift_locations(grid);
    let count = accessable_forklift_locations.len();
    format!("{count}")
}

pub fn p2(lines: Vec<String>) -> String {
    let grid = lines
        .iter()
        .map(|x| x.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let results = (0..)
        .scan(
            (0, grid),
            |(change_count, cur_grid), _| -> Option<(usize, Vec<Vec<char>>)> {
                let available_forklifts = get_accessable_forklift_locations(cur_grid.to_vec());
                let new_grid = remove_forklifts(cur_grid.to_vec(), available_forklifts.clone());
                if new_grid == cur_grid.to_vec() {
                    None
                } else {
                    *cur_grid = new_grid.clone();
                    *change_count += available_forklifts.len();
                    Some((*change_count, cur_grid.clone()))
                }
            },
        )
        .collect::<Vec<_>>();
    let (count, _) = results.last().unwrap();
    format!("{count}")
}

fn is_forklift(char: char) -> bool {
    char == '@'
}

fn get_accessable_forklift_locations(grid: Vec<Vec<char>>) -> Vec<(usize, usize)> {
    (0..grid.len())
        .flat_map(|x| {
            (0..grid[0].len())
                .filter_map(|y| {
                    if is_forklift(grid[x][y].to_ascii_lowercase()) {
                        let surrounding = get_surrounding(
                            x.try_into().unwrap(),
                            y.try_into().unwrap(),
                            grid.clone(),
                        );
                        let count = surrounding
                            .iter()
                            .filter(|f| is_forklift(**f))
                            .collect::<Vec<_>>()
                            .len();
                        if count < 4 { Some((x, y)) } else { None }
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn remove_forklifts(grid: Vec<Vec<char>>, coords_to_remove: Vec<(usize, usize)>) -> Vec<Vec<char>> {
    (0..grid.len())
        .map(|x| {
            (0..grid[0].len())
                .map(|y| {
                    if coords_to_remove.contains(&(x, y)) {
                        '.'.to_ascii_lowercase()
                    } else {
                        grid[x][y]
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn get_at(x: usize, y: usize, grid: Vec<Vec<char>>) -> Option<char> {
    grid.get(x).and_then(|z| z.get(y).copied())
}

fn get_surrounding(x: i32, y: i32, grid: Vec<Vec<char>>) -> Vec<char> {
    ((x - 1)..=(x + 1))
        .flat_map(|i| {
            ((y - 1)..=(y + 1))
                .filter_map(|j| {
                    // skip the space we're looking at or anything out of bounds on the left
                    if (i == x && j == y) || (i < 0 || j < 0) {
                        None
                    } else {
                        let i_usize = usize::try_from(i).unwrap();
                        let j_usize = usize::try_from(j).unwrap();
                        get_at(i_usize, j_usize, grid.clone())
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}
