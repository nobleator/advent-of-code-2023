module Day09
    let parseInput (str : string) =
        str.Split "\n"
        |> Array.map Day04.toIntArray

    let rec predict numbers =
        match numbers |> Array.exists (fun x -> not (x = 0)) with
        | true ->
            numbers
            |> Array.pairwise
            |> Array.map (fun (l, r) ->
                r - l
            )
            |> predict
            |> (+) (Array.last numbers)
        | false -> 0

    let part1 (str : string) =
        parseInput str
        |> Array.fold (fun acc row -> acc + predict row) 0

    let part2 (str : string) =
        0