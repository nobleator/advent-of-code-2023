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

    type MatchType =
    | SeedContained
    | SeedExcluded
    | IntersectionUpper
    | IntersectionLower
    | MapContained

    let splitSeeds seeds maps =
        seeds
        |> List.fold(fun acc seed ->
            let seedStart, seedRange = seed
            let seedEdge = seedStart + seedRange
            maps
            |> List.map (fun map ->
                let _, src, rng = map
                let mapEdge = src + rng
                (
                    match (seedStart, seedEdge) with
                    | (a, b) when a >= src && b <= mapEdge -> SeedContained
                    | (a, b) when (a < src && b < src) || (a > mapEdge && b > mapEdge) -> SeedExcluded
                    | (a, b) when a >= src && b > mapEdge -> IntersectionUpper
                    | (a, b) when a < src && b > src && b <= mapEdge -> IntersectionLower
                    | (a, b) when a < src && b >= mapEdge -> MapContained
                    | _ -> failwith "Seed could not be split"
                    , map
                )
            )
            |> List.collect (fun x ->
                let _, src, rng = x |> snd
                let mapEdge: int64 = src + rng
                match x |> fst with
                | SeedContained | SeedExcluded -> []
                | IntersectionUpper -> [(seedStart, mapEdge - seedStart - 1L); (mapEdge, seedEdge - mapEdge)]
                | IntersectionLower -> [(seedStart, src - seedStart - 1L); (src, seedEdge - src)]
                | MapContained -> [(seedStart, src - seedStart - 1L); (src, rng - 1L); (mapEdge, seedEdge - mapEdge)]
            )
            |> fun x -> 
                match x with
                | [] -> acc @ [(seedStart, seedRange - 1L)]
                | _ -> acc @ x
            |> List.sort
            |> List.distinctBy (fun x -> x |> fst)
        ) []

    let rec splitAndMap seedList mapList =
        match mapList with
        | currMap::restMaps ->
            splitSeeds seedList currMap
            |> List.map (fun curr -> 
                let currVal, range = curr
                currMap
                |> List.filter (fun m ->
                    let _, b, r = m
                    currVal >= b && currVal < (b + r)
                )
                |> fun x ->
                    match x with
                    | h::_ ->
                        let a, b, _ = h
                        (currVal - (b - a), range)
                    | [] -> (currVal, range)
            )
            |> fun newList -> splitAndMap newList restMaps
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
        splitAndMap s m
        |> List.minBy fst
        |> fst
