namespace Day1

open Input

module Program =
    
    let gt accumulator (current, previous) = previous < current, accumulator, current
    
    let inc (isGt, accumulator, current) =
        if isGt = true then accumulator + 1, current else accumulator, current
        
    let accumulate (accumulator, previous) current =
        gt accumulator (current, previous)
        |> inc
        
    let partition i =
        if i < Array.length input - 2
        then [input.[i]; input.[i + 1]; input.[i + 2];]
        else []
        
    let part2 =
        let indexedList = [0..Array.length input - 1]
        
        indexedList
        |> List.map partition
        |> List.filter (fun l -> not (List.isEmpty l))
        |> List.map List.sum
        |> List.toArray

    [<EntryPoint>]
    let main args =
        // part1 is the same but: Array.fold accumulate (0, 0) input
        Array.fold accumulate (0, 0) part2
        |> sprintf "%A"
        |> ignore

        0 // exit code