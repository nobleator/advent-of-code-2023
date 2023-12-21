module Day21
    open Day03

    let orthogonal p1 p2 =
        (abs(p1.x - p2.x) = 1 && abs(p1.y - p2.y) = 0) || (abs(p1.x - p2.x) = 0 && abs(p1.y - p2.y) = 1)

    let parseInput (str : string) =
        str.Split "\n"
        |> Array.map Seq.toList
        |> Array.mapi (fun yIdx r -> (yIdx, r))
        |> Array.fold (fun rowAcc row ->
            let yIdx, r = row
            let tmp =
                r
                |> List.mapi (fun xIdx c -> (xIdx, c))
                |> List.fold (fun colAcc col ->
                    let xIdx, c = col
                    colAcc @ [({x=xIdx; y=yIdx}, c)]
                ) []
            rowAcc @ tmp
        ) []

    let takeStep dirt = 
        let start = dirt |> Set.filter (fun x -> x |> snd = 'S' || x |> snd = 'O')
        dirt
        |> Set.map (fun x ->
            match start |> Set.exists (fun y -> orthogonal (x |> fst) (y |> fst)) with
            | true -> (x |> fst, 'O')
            | false -> (x |> fst, '.')
        )
    
    let part1 (str : string) goal =
        let _, dirt =
            parseInput str
            |> Set.ofList
            |> Set.partition (fun x -> x |> snd = '#')
        [1..goal]
        |> List.fold (fun acc i ->
            printfn $"stepping {i}"
            takeStep acc
        ) dirt
        |> Set.filter (fun x -> x |> snd = 'O')
        |> Set.count

    let part2 (str : string) =
        -1