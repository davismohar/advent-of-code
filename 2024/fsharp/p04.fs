module P04

let getWordStringsFromIndex (x: int, y: int) (grid: string list) = 
    let buildListOf (n: int) = seq { for _ in 0..3 do yield n} |> List.ofSeq
    let buildCoords (xList: int list, yList: int list) = seq { for i in 0..3 do (xList[i], yList[i])} |> List.ofSeq
    let getStringFromCoords (coords: (int * int) list) = 
        seq { for (x, y) in coords do grid[x][y]} 
        |> Seq.toArray 
        |> System.String.Concat
    let maxY = grid.Length - 1
    let maxX = grid.Head.Length - 1
    let negXRange = [(x-3)..x] |> List.rev
    let posXRange = [x..(x+3)]
    let negYRange = [(y-3)..y] |> List.rev
    let posYRange = [y..(y+3)]
    let staticXRange = buildListOf x
    let staticYRange = buildListOf y
    let directionPairs = [
        (staticXRange, posYRange)
        (posXRange, posYRange)
        (posXRange, staticYRange)
        (posXRange, negYRange)
        (staticXRange, negYRange)
        (negXRange, negYRange)
        (negXRange, staticYRange)
        (negXRange, posYRange)
    ]
    directionPairs 
    |> List.map buildCoords
    |> List.filter (fun coords ->
        let (fx, fy) = coords.Head
        let (lx, ly) = coords[3]
        fx >= 0 && fx <= maxX &&
        lx >= 0 && lx <= maxX &&
        fy >= 0 && fy <= maxY && 
        ly >= 0 && ly <= maxY 
    )
    |> List.map getStringFromCoords

let getXFromCoords (x: int, y: int) (grid: string list) =
    [grid[x-1][y-1]; grid[x][y]; grid[x+1][y+1];' '; grid[x-1][y+1];grid[x][y];grid[x+1][y-1]]
                |> System.String.Concat
    

let p04: Util.Puzzle = {
    name = "P04"
    part1 = (fun lines -> 
    let xMax = lines.Head.Length - 1
    let yMax = lines.Length - 1
    let xCoords = 
        seq { for x in 0..xMax  do seq { for y in  0..yMax do if lines[x][y] = 'X' then yield (x,y) }}
    xCoords 
        |> Seq.collect id 
        |> Seq.map (fun pair -> getWordStringsFromIndex pair lines)
        |> Seq.collect id
        |> Seq.filter (fun str -> str = "XMAS" )
        |> List.ofSeq
        |> List.length
        |> string
    
    )
    part2 = (fun lines -> 
    let xMax = lines.Head.Length - 1
    let yMax = lines.Length - 1
    seq { for x in 0..xMax  do seq { for y in  0..yMax do if lines[x][y] = 'A' then yield (x,y) }}
    |> Seq.collect id 
    |> Seq.filter (fun (x, y) -> x > 0 && y > 0 && x < xMax && y < yMax)
    |> Seq.map (fun pair -> getXFromCoords pair lines)
    |> Seq.filter ( fun s ->
        s = "MAS MAS" || s = "MAS SAM" || s = "SAM MAS" || s = "SAM SAM"
    )
    |> List.ofSeq |> List.length |> string
    )
}