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

    let parseInput (x : string) =
        match x.Split "\n\n" |> Array.toList with
        | h::t1 ->
            let seeds = match h.Split ':' |> Array.toList with
                        | _::t2 -> t2.Head |> toInt64Array
                        | _ -> failwith "Missing colon in seed row"
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

    let part1 (str : string) =
        let s, m = parseInput str
        s
        |> Array.fold (fun acc x->
            acc @ [(x, getUltimateMappedValue x m)]
        ) []
        |> List.minBy snd
        |> snd

    let part2 (x : string) =
        0