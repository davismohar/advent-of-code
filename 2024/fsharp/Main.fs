module Main

[<EntryPoint>]
let main(args) =    
    printfn "args: %A" args

    match args with
        | [|"P01"; mode|] -> Util.solve(P01.p01, mode)
        | [|"P02"; mode|] -> Util.solve(P02.p02, mode)
        | [|number; _|] -> failwith (sprintf "Puzzle %s is not implemented" number)
        | _ -> failwith (sprintf "Required input of {Puzzle Number} {Mode} (e.g. P01 input or P01 test)")
    0
