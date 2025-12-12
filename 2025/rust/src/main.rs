mod utils;
use clap::Parser;
use utils::util;
mod d01;
mod d02;
mod d03;
mod d04;
mod d05;

fn main() {
    let problems = [
        util::Problem {
            number: String::from("d01"),
            p1: d01::p1,
            p2: d01::p2,
        },
        util::Problem {
            number: String::from("d02"),
            p1: d02::p1,
            p2: d02::p2,
        },
        util::Problem {
            number: String::from("d03"),
            p1: d03::p1,
            p2: d03::p2,
        },
        util::Problem {
            number: String::from("d04"),
            p1: d04::p1,
            p2: d04::p2,
        },
        util::Problem {
            number: String::from("d05"),
            p1: d05::p1,
            p2: d05::p2,
        },
    ];
    let args = Args::parse();
    let problem_number = args.number.to_lowercase();
    let problem = problems
        .iter()
        .find(|p| p.number == problem_number)
        .expect("Problem number {problem_number} is not available");
    let part = args.part.to_lowercase();
    if !["p1", "p2"].contains(&part.as_str()) {
        panic!("{part} is not a valid problem part to solve. Must be p01 or p02")
    }
    let mode = args.mode.to_lowercase();
    let input = match mode.as_str() {
        "test" => util::read_test(problem_number.as_str(), part.as_str()).unwrap(),
        "main" => util::read_main(problem_number.as_str(), part.as_str()).unwrap(),
        _ => panic!("{mode} is not a valid mode. Must be `test` or `main`"),
    };
    let result = match part.as_str() {
        "p1" => (problem.p1)(input),
        "p2" => (problem.p2)(input),
        _ => panic!("how did you get here?"),
    };
    println!("{result}")
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    // problem number to run: e.g. d01
    #[arg(short, long)]
    number: String,

    // part to run: p1 or p2
    #[arg(short, long)]
    part: String,

    // mode to run: test or main
    #[arg(short, long)]
    mode: String,
}
