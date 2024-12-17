module P05

let swapIndexes (i1: int) (i2: int) (l: 'A list) =
    let p1 = l[0..i1-1]
    let p3 = l[i1+1..i2-1]
    let p5 = l[i2+1..l.Length-1]
    p1@l[i2]::p3@l[i1]::p5

let rulesForPageNumber (rules: Map<int, int list>) (pageNumber: int) = 
                    match (rules.TryFind pageNumber) with 
                    | None -> []
                    | Some value -> value

let indexToInsertAt (list: int list) (rulesForPageNum: int list) =
    match (List.tryFindIndex (fun num -> List.contains num rulesForPageNum) list) with 
            | None -> list.Length
            | Some len -> len


let insertAtIndex (list: int list) (index: int) (elem: int) =
    list[0..index-1]@elem::list[index..list.Length-1]

let insertIntoList (list: int list) (rules: Map<int, int list>) (elem: int)  =
    let insertI = indexToInsertAt list (rulesForPageNumber rules elem)
    insertAtIndex list insertI elem
    

let rec fixPageList (rules: Map<int, int list>) (pageList: int list) =
    List.fold (fun acc elem -> 
        let step = insertIntoList acc rules elem
        step
    ) [] pageList

let parseRules (lines: string list): Map<int, int list> =
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

let parsePagesToProduce(lines: string list) =
    lines 
    |> List.map (fun line -> line.Split ",")
    |> List.filter (fun parts -> parts.Length <> 1)
    |> List.map (fun parts -> parts |> List.ofArray |> List.map int)    
    
let getIllegalPages (pagesToProduce: (int list) list) (rules: Map<int, int list>) = 
     pagesToProduce 
        |> List.map (fun pageNumbers -> 
            let isIllegalList = pageNumbers |> List.mapi (fun i pageNumber -> 
                let before  = pageNumbers[0..i]
                let rulesForCurrentPageNumber = rulesForPageNumber rules pageNumber
                let isIllegal = 
                    before 
                    |> List.exists (fun prevPageNumber -> List.contains prevPageNumber rulesForCurrentPageNumber)
                isIllegal
            )
            let anyIllegal = isIllegalList |> List.exists id
            (pageNumbers, anyIllegal)
        )

let p05: Util.Puzzle  = {
    name = "P05"
    part1 = (fun lines -> 
    let rules = parseRules lines
    let pagesToProduce = parsePagesToProduce lines
    let pagesAndIsIllegal: ((int list) * bool ) list = getIllegalPages pagesToProduce rules
    let legalPages = pagesAndIsIllegal |> List.filter (fun (_, isIllegal) -> not isIllegal)
    legalPages 
    |> List.map (fun (l, _) -> l[l.Length/2])
    |> List.sum
    |> string
    )
    part2 = (fun lines -> 
    let rules = parseRules lines
    let pagesToProduce = parsePagesToProduce lines
    let pagesAndIsIllegal: ((int list) * bool ) list = getIllegalPages pagesToProduce rules
    let illegalPages = pagesAndIsIllegal |> List.filter (fun (_, isIllegal) ->  isIllegal) |> List.map (fun (l, _) -> l)
    let fixedPages = illegalPages |> List.map (fun plist -> fixPageList rules plist)
    fixedPages 
    |> List.map (fun l -> l[l.Length/2])
    |> List.sum
    |> string
    )
}