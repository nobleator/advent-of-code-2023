let readPuzzleInput dayNum =
    System.IO.File.ReadAllText $"./input/day{dayNum}.txt"

[<EntryPoint>]
let main args =
    match args |> Array.head |> int with
    | 1 -> readPuzzleInput "01" |> fun x -> Day01.calibrate x |> string
    | 2 -> readPuzzleInput "02" |> fun x -> Day02.part1 x |> string
    | _ -> "This day hasn't been written yet"
    |> fun x -> printfn $"{x}"
    0