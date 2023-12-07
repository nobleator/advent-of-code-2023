module Day06
    let parseInput (str : string) =
        str.Split [|'\n'|]
        |> Array.toList
        |> fun x ->
            match x with
            | h::t ->
                let times = 
                    h.Split ':'
                    |> Array.tail
                    |> Array.head
                    |> Day04.toIntArray
                    |> Array.toList
                let distances = 
                    t.Head.Split ':'
                    |> Array.tail
                    |> Array.head
                    |> Day04.toIntArray
                    |> Array.toList
                List.zip times distances
            | _ -> failwith "Unexpected input"

    let parseInput2 (str : string) =
        str.Split [|'\n'|]
        |> Array.toList
        |> fun x ->
            match x with
            | h::t ->
                let times = 
                    h.Split ':'
                    |> Array.tail
                    |> Array.head
                    |> fun x -> x.Split ' '
                    |> Array.filter (fun z -> not (System.String.IsNullOrWhiteSpace z))
                    |> String.concat ""
                    |> int64
                let distances = 
                    t.Head.Split ':'
                    |> Array.tail
                    |> Array.head
                    |> fun x -> x.Split ' '
                    |> Array.filter (fun z -> not (System.String.IsNullOrWhiteSpace z))
                    |> String.concat ""
                    |> int64
                (times, distances)
            | _ -> failwith "Unexpected input"

    let getWinningCount race =
        let time, dist = race
        [1..time]
        |> List.filter (fun x -> x * (time - x) > dist)
        |> List.length

    let getWinningCount64 race =
        let time, dist = race
        [1L..time]
        |> List.filter (fun x -> x * (time - x) > dist)
        |> List.length

    let part1 (str : string) =
        parseInput str |> List.fold (fun acc r -> acc * getWinningCount r) 1

    let part2 (str : string) =
        parseInput2 str |> getWinningCount64