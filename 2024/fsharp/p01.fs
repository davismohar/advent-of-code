module P01

let part1 (lines: string list) = 
    let splitLines =  
        lines 
        |> List.map ((Util.splitLineToInts) "   ")

    let list1 = 
        splitLines 
        |> List.map (fun x -> x[0])
        |> List.sort
    let list2= 
        splitLines 
        |> List.map (fun x -> x[1])
        |> List.sort

    let zipped =  
        List.zip list1 list2 
        |> List.map (fun (a, b) -> abs(a - b))
    
    let sum = zipped |> List.sum 
    sum.ToString()


let part2 (lines: string list) = 
    let splitLines =  
        lines 
        |> List.map ((Util.splitLineToInts) "   ")

    let list1 = 
        splitLines 
        |> List.map (fun x -> x[0])
    let list2= 
        splitLines 
        |> List.map (fun x -> x[1])

    let getNumberCounts(list: int list): Map<int, int> = 
        List.fold (fun (map: Map<int, int>) (elem: int) ->
            let nextVal =
                match map.TryFind elem with
                    | Some currVal -> currVal + 1
                    | None -> 1
            map.Add(elem, nextVal)
        )
            Map[]
            list
        

    let list2Counts = getNumberCounts(list2)

    let similarityScores = 
        list1 
        |> List.map (fun number -> 
                match list2Counts.TryFind(number) with
                    | Some count -> number * count
                    | None -> 0
            )
    similarityScores |> List.sum |> (fun x -> x.ToString())


let p01: Util.Puzzle = {
    name = "P01"; 
    part1 = part1;
    part2 = part2;
    }









