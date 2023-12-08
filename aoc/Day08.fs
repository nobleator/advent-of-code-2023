module Day08
    let parseInput (str : string) =
        match str.Split "\n\n" |> Array.toList with
        | h::t ->
            let instructions = h |> Seq.toList
            let network =
                t.Head.Split "\n"
                |> Array.toList
                |> List.map (fun x ->
                    match x.Split '=' |> Array.toList with
                    | l::r ->
                        let a = l.Trim ' '
                        let b = r.Head.Trim().TrimStart('(').TrimEnd(')').Split(',') |> Array.toList
                        match b with
                        | dl::dr -> (a, (dl.Trim(), dr.Head.Trim()))
                        | _ -> failwith "Incomplete network node definition on rhs"
                    | _ -> failwith "Incomplete network node definition on equals"
                )
                |> Map.ofList
            (instructions, network)
        | _ -> failwith "Input missing double newlines"

    let rec traverse depth currNode network instructions  originalInstructions =
        let newDepth = depth + 1
        match instructions with
        | currInst::restInst ->
            let nextNode : string =
                match currInst with
                | 'L' -> Map.find currNode network |> fst
                | 'R' -> Map.find currNode network |> snd
                | _ -> failwith "Unexpected instruction"
            match nextNode.EndsWith('Z') with
            | true -> newDepth
            | _ -> traverse newDepth nextNode network restInst originalInstructions
        | _ -> traverse depth currNode network originalInstructions originalInstructions

    let part1 (str : string) =
        let i, n = parseInput str
        traverse 0 "AAA" n i i

    let part2 (str : string) =
        let i, n = parseInput str
        n.Keys 
        |> Seq.toList
        |> List.filter (fun x -> x.EndsWith('A'))
        |> List.map (fun node -> traverse 0 node n i i |> int64)
        |> List.toArray
        |> MathNet.Numerics.Euclid.LeastCommonMultiple