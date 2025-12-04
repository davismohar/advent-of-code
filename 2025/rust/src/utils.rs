pub mod util {
    use std::fs;
    use std::io;
    pub fn read_lines(filename: String) -> io::Result<Vec<String>> {
        let contents = fs::read_to_string(filename)?;
        let lines = contents
            .split('\n')
            .map(|x| x.to_owned().to_string())
            .collect::<Vec<String>>();
        Ok(lines)
    }

    pub fn read_test(day_number: &str, part: &str) -> io::Result<Vec<String>> {
        read_lines(format!("input/{day_number}/{part}/test.txt"))
    }

    pub fn read_main(day_number: &str, part: &str) -> io::Result<Vec<String>> {
        read_lines(format!("input/{day_number}/{part}/input.txt"))
    }

    pub struct Problem {
        pub number: String,
        pub p1: fn(Vec<String>) -> String,
        pub p2: fn(Vec<String>) -> String,
    }
}
