module Day20
    type Pulse =
    | Low
    | High

    type State =
    | Off
    | On
    
    let parseInput (str : string) =
        str.Split "\n"
        |> Array.fold (fun acc row ->
            let types, children = acc
            match row.Split "->" with
            | [| lhs; rhs; |] ->
                let targets =
                    rhs.Split ","
                    |> Array.map (fun x -> x.Trim())
                    |> Array.toList
                match lhs |> Seq.toList with
                | h::_ when h = 'b' ->
                    let k = lhs.Trim()
                    (Map.add k h types, Map.add k targets children)
                | h::t ->
                    let k = t |> List.map string |> String.concat "" |> fun x -> x.Trim()
                    (Map.add k h types, Map.add k targets children)
                | _ -> failwith "Insufficient Christmas cheer"
            | _ -> failwith "Expected 2 sides to module definitions"
        ) (Map.empty, Map.empty)

    let addOrUpdate k v m =
        match Map.containsKey k m with
        | false -> Map.add k v m
        | true ->
            Map.change k (fun x ->
                match x with
                | Some _ -> Some (v)
                | None -> None
            ) m

    let rec pulse typesConfig childrenConfig queue ffState conjState seen pulseCounts maxIterations counter =
        let rec inner queue ffState conjState seen pulseCounts =
            match queue with
            | node::t ->
                let priorModule, currentModule, incomingPulse = node
                match priorModule with
                | "mr" when currentModule = "qt" && incomingPulse = High -> printfn $"found match for mr: {counter}"
                | "kk" when currentModule = "qt" && incomingPulse = High -> printfn $"found match for kk: {counter}"
                | "gl" when currentModule = "qt" && incomingPulse = High -> printfn $"found match for gl: {counter}"
                | "bb" when currentModule = "qt" && incomingPulse = High -> printfn $"found match for bb: {counter}"
                | _ -> ()
                |> ignore
                let pulseCounts' =
                    match incomingPulse with
                    | Low -> ((pulseCounts |> fst)+1, pulseCounts |> snd)
                    | High -> (pulseCounts |> fst, (pulseCounts |> snd)+1)
                let moduleTypeOpt = Map.tryFind currentModule typesConfig
                match moduleTypeOpt with
                | None -> inner t ffState conjState seen pulseCounts'
                | Some moduleType -> 
                    match Set.contains (ffState, conjState) seen with
                    | true -> (ffState, conjState, pulseCounts')
                    | false ->
                        // let seen' = Set.add (ffState, conjState) seen
                        let seen' = seen
                        let children = Map.find currentModule childrenConfig
                        match moduleType with
                        | 'b' ->
                            let q' = t @ (children |> List.map (fun c -> (currentModule, c, Low)))
                            inner q' ffState conjState seen pulseCounts'
                        | '%' ->
                            match incomingPulse with
                            | High -> inner t ffState conjState seen pulseCounts'
                            | Low ->
                                match Map.find currentModule ffState with
                                | Off ->
                                    let state' = addOrUpdate currentModule On ffState
                                    let q' = t @ (children |> List.map (fun c -> (currentModule, c, High)))
                                    inner q' state' conjState seen' pulseCounts'
                                | On ->
                                    let state' = addOrUpdate currentModule Off ffState
                                    let q' = t @ (children |> List.map (fun c -> (currentModule, c, Low)))
                                    inner q' state' conjState seen' pulseCounts'
                        | '&' ->
                            let priorState = Map.find currentModule conjState
                            let priorState' = addOrUpdate priorModule incomingPulse priorState
                            let conjState' = addOrUpdate currentModule priorState' conjState
                            let allHigh = priorState' |> Map.forall (fun _ v -> v = High)
                            let q' =
                                match allHigh with
                                | true -> t @ (children |> List.map (fun c -> (currentModule, c, Low)))
                                | false -> t @ (children |> List.map (fun c -> (currentModule, c, High)))
                            inner q' ffState conjState' seen' pulseCounts'
                        | _ -> (ffState, conjState, pulseCounts')
            | _ -> (ffState, conjState, pulseCounts)
        let ffState', conjState', pulseCounts' = inner queue ffState conjState seen pulseCounts
        match counter < maxIterations with
        | true -> pulse typesConfig childrenConfig queue ffState' conjState' seen pulseCounts' maxIterations (counter+1)
        | false -> (ffState', conjState', pulseCounts')

    let part1 (str : string) =
        let types, children = parseInput str
        let ffState = Map.keys types |> Seq.map (fun k -> (k, Off)) |> Map.ofSeq
        let conjState =
            Map.keys types
            |> Seq.map (fun k ->
                let parents =
                    Map.filter (fun _ c -> c |> List.contains k)children
                    |> Map.keys
                    |> Seq.toList
                (k, parents |> List.map (fun p -> (p, Low)) |> Map.ofList)
            )
            |> Map.ofSeq
        let start = ("", "broadcaster", Low)
        let (_, _, pulseCounts) = pulse types children [start] ffState conjState Set.empty (0,0) 1000 1
        (pulseCounts |> fst) * (pulseCounts |> snd)

    let part2 (str : string) =
        // Find nodes pointing to rx by manual inspection
        // &mr, &kk, &gl, and &bb -> qt, and &qt -> rx
        // For each of these nodes, find the cycle at which it will output High
        let types, children = parseInput str
        let ffState = Map.keys types |> Seq.map (fun k -> (k, Off)) |> Map.ofSeq
        let conjState =
            Map.keys types
            |> Seq.map (fun k ->
                let parents =
                    Map.filter (fun _ c -> c |> List.contains k)children
                    |> Map.keys
                    |> Seq.toList
                (k, parents |> List.map (fun p -> (p, Low)) |> Map.ofList)
            )
            |> Map.ofSeq
        let start = ("", "broadcaster", Low)
        let (_, _, _) = pulse types children [start] ffState conjState Set.empty (0,0) 5000 1
        // The LCM of all of these cycles will identify when they sync
        [| 3907L; 3931L; 3967L; 3989L; |] |> MathNet.Numerics.Euclid.LeastCommonMultiple