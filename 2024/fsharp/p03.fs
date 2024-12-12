module P03

let mulRegex = System.Text.RegularExpressions.Regex("mul\(\d+,\d+\)")
let numberRegex = System.Text.RegularExpressions.Regex("\d+")

type MultiplicationPair = { X: int; Y: int; Index: int }

let toList (f: System.Text.RegularExpressions.Match -> 'a) (s: System.Text.RegularExpressions.MatchCollection) =
    seq {
        for m in s do
            yield f m
    }
    |> List.ofSeq

let parseNumbersFromLine (line: string) =
    mulRegex.Matches line
    |> (toList) (fun x -> (x.Value, x.Index))
    |> List.map (fun (multString, index) ->
        let reMatches = multString |> numberRegex.Matches |> (toList) (fun x -> int x.Value)

        { X = reMatches[0]
          Y = reMatches[1]
          Index = index })

type Mode =
    | Enabled
    | Disabled

let superRegex =
    System.Text.RegularExpressions.Regex("(?<Mul>mul\(\d+,\d+\))|(?<Do>do\(\))|(?<Dont>don't\(\))")

let rec p2 (currentMode: Mode) (sum: int) (remainingString: string) =
    let reMatch = superRegex.Match(remainingString)

    let matchedGroup =
        reMatch.Groups
        |> List.ofSeq
        |> List.filter (fun g -> g.Success)
        |> List.filter (fun g -> g.Name <> "0")
        |> List.map (fun g -> g.Name)
        |> List.tryHead

    match (matchedGroup, currentMode) with
    | (None, _) -> (currentMode, sum)
    | (Some "Do", _) -> p2 Mode.Enabled sum (remainingString[reMatch.Index + 1 .. remainingString.Length - 1])
    | (Some "Dont", _) -> p2 Mode.Disabled sum (remainingString[reMatch.Index + 1 .. remainingString.Length - 1])
    | (Some "Mul", Mode.Disabled) ->
        p2 currentMode sum (remainingString[reMatch.Index + 1 .. remainingString.Length - 1])
    | (Some "Mul", Mode.Enabled) ->
        let nums = reMatch.Value |> numberRegex.Matches |> (toList) (fun x -> int x.Value)
        let product = nums[0] * nums[1]
        p2 currentMode (sum + product) (remainingString[reMatch.Index + 1 .. remainingString.Length - 1])
    | _ -> failwith "No matched group"


let p03: Util.Puzzle =
    { name = "P03"
      part1 =
        (fun lines ->
            lines
            |> List.map parseNumbersFromLine
            |> List.collect id
            |> List.map (fun { X = x; Y = y } -> x * y)
            |> List.sum
            |> string)
      part2 =
        (fun lines ->
            let (_, num) =
                List.fold (fun (mode, sum) line -> p2 mode sum line) (Enabled, 0) lines

            string num) }
