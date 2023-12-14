module Day12
    let unfoldInput springs groups  =
        let newSprings = [0..4] |> List.collect (fun n ->
            match n with
            | 4 -> springs
            | _ -> springs @ ['?']
        )
        let newGroups = [|0..4|] |> Array.collect (fun _ -> groups)
        (newSprings, newGroups)
    
    let parseInput (str : string) =
        str.Split "\n"
        |> Array.map (fun x ->
            x.Split ' '
            |> Array.toList
            |> fun x -> 
                match x with
                | h::t -> (h |> Seq.toList, t.Head.Split ',' |> Array.map int)
                | _ -> failwith "2 elements expected"
        )

    let trySkipWithDefault skipSize skipList =
        match List.tryItem skipSize skipList with
        | Some _ -> List.skip skipSize skipList
        | None -> []

    let cache = new System.Collections.Generic.Dictionary<System.Tuple<string, string>, int64>(HashIdentity.Structural);

    let rec memoizedCheck x y =
        let inner springs groups =
            match springs with
            | h::t ->
                match Array.length groups = 0 with
                | true ->
                    match List.contains '#' springs with
                    | true -> 0L
                    | false -> 1L
                | false -> 
                    let groupSize = Array.head groups
                    let validStretch =
                        groupSize <= List.length springs &&
                        List.take groupSize springs |> List.forall (fun c -> c <> '.') &&
                        (groupSize = List.length springs || List.item groupSize springs <> '#')
                    match h with
                    | '.' -> memoizedCheck t groups
                    | '?' when not validStretch-> memoizedCheck t groups
                    | '?' when validStretch -> memoizedCheck t groups + memoizedCheck (trySkipWithDefault (groupSize + 1) springs) (Array.tail groups)
                    | '#' when not validStretch -> 0L
                    | '#' when validStretch -> memoizedCheck (trySkipWithDefault (groupSize + 1) springs) (Array.tail groups)
                    | _ -> failwith "Springs should always match . ? or #"
            | [] ->
                match Array.length groups = 0 with
                | true -> 1L
                | false -> 0L
        let k = (x |> List.map string |> String.concat "", y |> Array.map string |> String.concat "|")
        match cache.TryGetValue k with
        | true, v -> v
        | _ -> 
            let v = inner x y
            cache.Add(k, v)
            v

    let part1 (str : string) =
        parseInput str
        |> Array.fold (fun acc x ->
            let springs, groups = x
            acc + memoizedCheck springs groups
        ) 0L

    let part2 (str : string) =
        parseInput str
        |> Array.map (fun x ->
            let springs, groups = x
            unfoldInput springs groups
        )
        |> Array.fold (fun acc x ->
            let springs, groups = x
            acc + memoizedCheck springs groups
        ) 0L
