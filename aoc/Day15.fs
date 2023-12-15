module Day15
    let parseInput (str : string) =
        str.Split ','
        |> Array.map Seq.toList
    
    let hash chars =
        chars |> List.fold (fun acc c -> ((acc + (c |> int)) * 17) % 256) 0

    let part1 (str : string) =
        parseInput str |> Array.fold (fun acc x -> acc + hash x) 0

    let part2 (str : string, expansionFactor : int) =
        -1