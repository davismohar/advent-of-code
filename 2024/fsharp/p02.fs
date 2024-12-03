module P02

[<TailCall>]
let rec isTrueForAll (predicate: int -> int -> bool) (previous: Option<int>) (list: int list) =
    match (list, previous) with
        | ([], _ ) -> true
        | (head:: tail, None) -> isTrueForAll predicate (Some(head)) tail 
        | (head :: tail, Some prev) when (predicate prev head) -> isTrueForAll predicate (Some(head)) (tail) 
        | _ -> false

let isIncreasing: int option -> int list -> bool = (isTrueForAll (>) )
let isDecreasing : int option -> int list -> bool = (isTrueForAll (<) )

let levelsBetweenOneAndThree : int option -> int list -> bool = (isTrueForAll (fun x y -> abs(x-y) <= 3))

let isSafe x =  
    let inc = isIncreasing None x
    let dec = isDecreasing None x
    let levels = levelsBetweenOneAndThree None x
    (inc || dec) && levels

[<TailCall>]
let rec remove i l =
    match i, l with
    | 0, x::xs -> xs
    | i, x::xs -> x::remove (i - 1) xs
    | i, [] -> failwith "index out of range"

[<TailCall>]
let rec isSafeMinus (removedI: int) (nums: int list) =
    if (removedI = nums.Length) then false
    else if isSafe (remove removedI nums) then true
    else isSafeMinus (removedI + 1) (nums)

let rec isSafeWithRemoved (nums: int list) = isSafeMinus 0 nums

let part1 (input: string list) : string =
    let reports = input |> List.map (Util.splitLineToInts " ")
    let checks =    reports |> List.map isSafe
    let count = checks |> List.filter id |> List.length
    string count

let part2 (input: string list) : string =
    let reports = input |> List.map (Util.splitLineToInts " ")
    let checks = reports |> List.map isSafeWithRemoved
    let count = checks |> List.filter id |> List.length
    string count

let p02: Util.Puzzle = {
    name = "P02"; 
    part1 = part1;
    part2 = part2;
    }


