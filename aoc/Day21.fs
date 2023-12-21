module Day21
    open Day03
    open Day11

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

    let takeStep (start, count, dirt, seen) = 
        let priorState = dirt |> Set.filter (fun x -> x |> snd = 'S' || x |> snd = 'O')
        let dirt' =
            dirt
            |> Set.filter (fun x -> not (Set.contains x seen))
            |> Set.map (fun x ->
                match priorState |> Set.exists (fun y -> orthogonal (x |> fst) (y |> fst)) with
                | true -> (x |> fst, 'O')
                | false -> (x |> fst, '.')
            )
        let seen' = Set.union (dirt' |> Set.filter (fun x -> x |> snd = 'S' || x |> snd = 'O')) seen
        let count' = seen' |> Set.filter (fun x -> distance ((x |> fst), start) % 2 = 0) |> Set.count
        (start, count', dirt', seen')
    
    let part1 (str : string) goal =
        let start =
            parseInput str
            |> List.filter (fun x -> x |> snd = 'S')
            |> List.exactlyOne
            |> fst
        let _, dirt =
            parseInput str
            |> Set.ofList
            |> Set.partition (fun x -> x |> snd = '#')
        [1..goal]
        |> List.fold (fun acc i ->
            printfn $"stepping {i}"
            takeStep acc
        ) (start, 0, dirt, Set.empty)
        |> fun (_, c, _, _) -> c

    let part2 (str : string) goal =
        -1