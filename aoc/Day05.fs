module Day05
    let toInt64Array (str : string) =
        str.Split ' '
        |> Array.filter (fun z -> not (System.String.IsNullOrWhiteSpace z))
        |> Array.map int64
    
    let parseMaps (str : string) =
        match str.Split '\n' |> Array.toList with
        | _::t2 ->
            t2
            |> List.fold (fun acc y -> 
                match y |> toInt64Array with
                | [|a; b; r|] -> acc @ [(a, b, r)]
                | _ -> failwith "Mapping row doesn't have the expected 3 numbers"
            ) []
        | _ -> failwith "Missing newline in mapping section"

    let rec buildMaps acc x =
        match x with
        | h::t -> buildMaps (acc @ [parseMaps h]) t
        | [] -> acc

    let parseSeeds (x : string) =
        match x.Split ':' |> Array.toList with
        | _::t2 -> t2.Head |> toInt64Array
        | _ -> failwith "Missing colon in seed row"

    let parseInput (x : string) =
        match x.Split "\n\n" |> Array.toList with
        | h::t1 ->
            let seeds = parseSeeds h
            let maps = buildMaps [] t1
            (seeds, maps)
        | _ -> failwith "Input missing double newlines"

    let parseSeeds2  (x : string) =
        match x.Split ':' |> Array.toList with
        | _::t2 ->
            t2.Head
            |> toInt64Array
            |> Array.chunkBySize 2
            |> Array.map (fun x ->
                match x with
                | [|a; b|] -> (a, b)
                | _ -> failwith "Unexpected chunk result"
            )
            |> Array.toList
        | _ -> failwith "Missing colon in seed row"

    let parseInput2 (x : string) =
        match x.Split "\n\n" |> Array.toList with
        | h::t1 ->
            let seeds = parseSeeds2 h
            let maps = buildMaps [] t1
            (seeds, maps)
        | _ -> failwith "Input missing double newlines"

    let rec getUltimateMappedValue i m =
        match m with
        | h::t ->
            let j =
                h
                |> List.filter (fun x ->
                    let _, b, r = x
                    i > b && i < (b + r)
                )
                |> fun x ->
                    match x with
                        | h::_ ->
                            let a, b, _ = h
                            i - (b - a)
                        | [] -> i
            getUltimateMappedValue j t
        | _ -> i

    let splitSeeds (seeds : (int64 * int64) list, maps) =
        seeds
        |> List.fold(fun seedList seed ->
            let seedStart, seedRange = seed
            let seedEdge = seedStart + seedRange
            let splits =
                maps
                |> List.fold (fun acc map ->
                    let dest, src, rng = map
                    let mapEdge = src + rng
                    match (seedStart, seedEdge) with
                    | (a, b) when a >= src && a < mapEdge -> acc @ [(seedStart - src + dest, seedEdge - src + dest);]
                    | (a, b) when a < src && b >= src && b < mapEdge -> acc @ [(dest, dest + seedEdge - src);]
                    | (a, b) when a >= src && a < mapEdge && b >= mapEdge && b < mapEdge -> acc @ [(dest + seedStart - src, dest + rng - 1)]
                    | (a, b) when a < src && b >= mapEdge -> acc @ [(dest, dest + rng - 1)]
                    // TODO need to avoid adding duplicates
                    | _ -> acc @ [(seedStart, seedRange)]
                ) []
                // |> Set.ofList
                // |> Set.toList
            seedList @ splits
        ) []

    let rec foo seedList mapList =
        match mapList with
        | currMap::restMaps ->
            let splitSeedList = splitSeeds seedList currMap
            let newSeedList =
                splitSeedList
                |> List.map (fun curr -> 
                    let currVal, range = curr
                    currMap
                    |> List.filter (fun m ->
                        let _, b, r = m
                        currVal > b && currVal < (b + r)
                    )
                    |> fun x ->
                        match x with
                            | h::_ ->
                                let a, b, _ = h
                                (currVal - (b - a), range)
                            | [] -> (currVal, range)
                )
            foo newSeedList restMaps
        | _ -> seedList

    let part1 (str : string) =
        let s, m = parseInput str
        s
        |> Array.fold (fun acc x->
            acc @ [(x, getUltimateMappedValue x m)]
        ) []
        |> List.minBy snd
        |> snd

    let part2 (str : string) =
        let s, m = parseInput2 str
        // let tmp = splitSeeds s m
        foo s m
        |> List.minBy fst
        |> fst