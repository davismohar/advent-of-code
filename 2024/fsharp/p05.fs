module P05

let p05: Util.Puzzle  = {
    name = "P05"
    part1 = (fun lines -> 
    let rules: Map<int, int list> = 
        lines
        |> List.map (fun line -> line.Split "|")
        |> List.filter (fun parts -> parts.Length = 2)
        |> List.map (fun parts -> (int parts[0], int parts[1]))
        |> List.fold (fun acc (x, y) -> 
            let existingList = acc.TryFind x
            match (existingList) with
            | None -> acc.Add (x, [y])
            | Some l -> acc.Add(x, y::l)
        ) (Map<int, int list>[])

    let pagesToProduce = 
        lines
        |> List.map (fun line -> line.Split ",")
        |> List.filter (fun parts -> parts.Length <> 1)
        |> List.map (fun parts -> parts |> List.ofArray |> List.map int)    
    
    let pagesAndIsIllegal: ((int list) * bool ) list = 
        pagesToProduce 
        |> List.map (fun pageNumbers -> 
            let isIllegalList = pageNumbers |> List.mapi (fun i pageNumber -> 
                let before  = pageNumbers[0..i]
                let rulesForCurrentPageNumber = 
                    match (rules.TryFind pageNumber) with 
                    | None -> []
                    | Some value -> value
                let isIllegal = 
                    before 
                    |> List.exists (fun prevPageNumber -> List.contains prevPageNumber rulesForCurrentPageNumber)
                isIllegal
            )
            let anyIllegal = isIllegalList |> List.exists id
            (pageNumbers, anyIllegal)
        )
    let legalPages = pagesAndIsIllegal |> List.filter (fun (_, isIllegal) -> not isIllegal)
    legalPages 
    |> List.map (fun (l, _) -> l[l.Length/2])
    |> List.sum
    |> string
    )
    part2 = (fun lines -> "")
}