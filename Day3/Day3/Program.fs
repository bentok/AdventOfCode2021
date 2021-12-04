module Program

open Input

let selectEpsilon (pair: List<char * int>) =
    let a, b = pair.[0]
    let c, d = pair.[1]
    if b > d then c else a
    
let selectGamma (pair: List<char * int>) =
    let a, b = pair.[0]
    let c, d = pair.[1]
    if b > d then a else c

let toDecimal input rateType =
    input
    |> Array.toList
    |> List.map (fun (a: string) -> Seq.toList a)
    |> List.transpose
    |> List.map (fun a -> a |> List.countBy id)
    |> if rateType = "gamma"
        then List.map selectGamma
        else List.map selectEpsilon
    |> Array.ofList
    |> System.String.Concat
    |> (fun a -> System.Convert.ToInt32(a.ToString(), 2))
    
toDecimal input "gamma" * toDecimal input "epsilon" |> (fun a -> printf $"{a}")
