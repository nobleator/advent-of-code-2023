let readPuzzleInput dayNum =
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
        Day09.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 10 -> readPuzzleInput "10" |> fun x ->
        Day10.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day10.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 11 -> readPuzzleInput "11" |> fun x ->
        Day11.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day11.part2 (x, 1000000) |> fun y -> printfn $"Part 2: {y}"
    | 12 -> readPuzzleInput "12" |> fun x ->
        Day12.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day12.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 13 -> readPuzzleInput "13" |> fun x ->
        Day13.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day13.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 14 -> readPuzzleInput "14" |> fun x ->
        Day14.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day14.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 15 -> readPuzzleInput "15" |> fun x ->
        Day15.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day15.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 16 -> readPuzzleInput "16" |> fun x ->
        Day16.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day16.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 17 -> readPuzzleInput "17" |> fun x ->
        Day17.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day17.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 18 -> readPuzzleInput "18" |> fun x ->
        Day18.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day18.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 19 -> readPuzzleInput "19" |> fun x ->
        Day19.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day19.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 20 -> readPuzzleInput "20" |> fun x ->
        Day20.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day20.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 21 -> readPuzzleInput "21" |> fun x ->
        Day21.part1 x 64 |> fun y -> printfn $"Part 1: {y}"
        Day21.part2 x 26501365L |> fun y -> printfn $"Part 2: {y}"
    | 22 -> readPuzzleInput "22" |> fun x ->
        Day22.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day22.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 23 -> readPuzzleInput "23" |> fun x ->
        Day23.part1 x |> fun y -> printfn $"Part 1: {y}"
        Day23.part2 x |> fun y -> printfn $"Part 2: {y}"
    | 24 -> readPuzzleInput "24" |> fun x ->
        Day24.part1 x (200000000000000.0,400000000000000.0) |> fun y -> printfn $"Part 1: {y}"
        // Day24.part2 x |> fun y -> printfn $"Part 2: {y}"
    | _ -> failwith "This day hasn't been written yet"
    0