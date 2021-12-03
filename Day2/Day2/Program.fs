module Day2

open Input

module Program =
    let distance (a: string) =
        a.Split(" ").[1]
        |> int

    let move (x: int, y: int) (current: string) =
        match current with
        | a when a.Contains("up") -> x, y - distance current
        | a when a.Contains("down") -> x, y + distance current
        | _ -> x + distance current, y
        
    let calculateJourney input =
        input
        |> Array.fold move (0, 0)
        |> (fun (x, y) -> x * y)

    [<EntryPoint>]
    let main argv =
        calculateJourney input
        |> (fun a -> printf $"{a}")
    
        0 