﻿let readPuzzleInput dayNum =
    System.IO.File.ReadAllText $"./input/day{dayNum}.txt"

[<EntryPoint>]
let main args =
    match args |> Array.head |> int with
    | 1 -> readPuzzleInput "01" |> fun x -> Day01.calibrate x |> string |> fun x -> printfn $"{x}"
    | 2 -> readPuzzleInput "02" |> fun x ->
        Day02.part1 x |> fun y -> printfn $"{y}"
        Day02.part2 x |> fun y -> printfn $"{y}"
    | 3 -> readPuzzleInput "03" |> fun x ->
        Day03.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day03.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 4 -> readPuzzleInput "04" |> fun x ->
        Day04.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day04.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 5 -> readPuzzleInput "05" |> fun x ->
        Day05.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day05.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 6 -> readPuzzleInput "06" |> fun x ->
        Day06.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day06.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 7 -> readPuzzleInput "07" |> fun x ->
        Day07.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day07.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 8 -> readPuzzleInput "08" |> fun x ->
        Day08.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day08.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 9 -> readPuzzleInput "09" |> fun x ->
        Day09.part1 x |> fun y -> printfn $"Part 1: {y}"
        // Day09.part2 x |> fun y -> printfn $"Part 2: {y}"
    | _ -> failwith "This day hasn't been written yet"
    0