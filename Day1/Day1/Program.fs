namespace Day1

open Input

module Program =
    
    let gt accumulator (current, previous) = previous < current, accumulator, current
    
    let inc (isGt, accumulator, current) =
        if isGt = true then accumulator + 1, current else accumulator, current
        
    let accumulate (accumulator, previous) current =
        gt accumulator (current, previous)
        |> inc

    [<EntryPoint>]
    let main args =
        Array.fold accumulate (0, 0) input
        |> sprintf "%A"
        |> ignore

        0 // exit code