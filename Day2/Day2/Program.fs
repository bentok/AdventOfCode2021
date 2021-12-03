module Day2

open Input

module Program =
    let distance (a: string) =
        a.Split(" ").[1]
        |> int

    let move (x: int, y: int, z: int) (current: string) =
        match current with
        | a when a.Contains("up") -> x, y - distance current, z
        | a when a.Contains("down") -> x, y + distance current, z
        | _ -> x + distance current, y, z + distance current * y
        
    let calculateJourney input =
        input
        |> Array.fold move (0, 0, 0)
        |> (fun (x, _, z) -> x * z)

    [<EntryPoint>]
    let main argv =
        calculateJourney input
        |> (fun a -> printf $"{a}")
    
        0 