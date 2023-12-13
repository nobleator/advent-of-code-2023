module Day12
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

    let rec check springs groups =
        match springs with
        | h::t ->
            match Array.length groups = 0 with
            | true ->
                match List.contains '#' springs with
                | true -> 0
                | false -> 1
            | false -> 
                let groupSize = Array.head groups
                let validStretch =
                    groupSize <= List.length springs &&
                    List.take groupSize springs |> List.forall (fun c -> c <> '.') &&
                    (groupSize = List.length springs || List.item groupSize springs <> '#')
                match h with
                | '.' -> check t groups
                | '?' when not validStretch-> check t groups
                | '?' when validStretch -> check t groups + check (trySkipWithDefault (groupSize + 1) springs) (Array.tail groups)
                | '#' when not validStretch -> 0
                | '#' when validStretch -> check (trySkipWithDefault (groupSize + 1) springs) (Array.tail groups)
                | _ -> failwith "Springs should always match . ? or #"
        | [] ->
            match Array.length groups = 0 with
            | true -> 1
            | false -> 0

    let part1 (str : string) =
        parseInput str
        |> Array.fold (fun acc x ->
            let springs, groups = x
            acc + check springs groups
        ) 0

    let part2 (str : string) =
        -1