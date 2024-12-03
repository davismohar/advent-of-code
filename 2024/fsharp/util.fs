module Util 
type Puzzle = { 
    name: string; 
    part1: list<string> -> string; 
    part2: list<string> -> string;
    }

let solution(puzzle: Puzzle, mode: string): string * string =
    let inputPath = sprintf "input/%s/%s.txt" puzzle.name mode
    let input= inputPath |> System.IO.File.ReadLines |> List.ofSeq
    let p1 = input |> puzzle.part1
    let p2 =  input |> puzzle.part2
    p1, p2

let solve (puzzle: Puzzle, mode: string) =
    printfn "---%s %s---" puzzle.name mode
    let (p1, p2) = solution(puzzle, mode)
    printfn "Part 1: %s" p1
    printfn "Part 2: %s" p2

let splitLineToInts (separator: string) (line: string) = 
    Array.toList(line.Split(separator))
    |> List.map int