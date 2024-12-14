module Main

[<EntryPoint>]
let main(args) =    
    match args with
        | [|"P01"; mode|] -> Util.solve(P01.p01, mode)
        | [|"P02"; mode|] -> Util.solve(P02.p02, mode)
        | [|"P03"; mode|] -> Util.solve(P03.p03, mode)
        | [|"P04"; mode|] -> Util.solve(P04.p04, mode)
        | [|"P05"; mode|] -> Util.solve(P05.p05, mode)
        | [|number; _|] -> failwith (sprintf "Puzzle %s is not implemented" number)
        | _ -> failwith (sprintf "Required input of {Puzzle Number} {Mode} (e.g. P01 input or P01 test)")
    0
