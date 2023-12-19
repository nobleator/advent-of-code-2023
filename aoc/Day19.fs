module Day19
    type Operator =
    | None
    | Lt
    | Gt
    
    let parseInput (str : string) =
        let tmp = str.Split "\n\n"
        let workflows =
            tmp
            |> Array.head
            |> fun s -> s.Split "\n"
            |> Array.fold (fun acc wf ->
                match wf.Split "{" with
                | [| k; rhs; |] ->
                    let v =
                        rhs.Split ","
                        |> Array.toList
                        |> List.map (fun x -> x.TrimEnd('}'))
                        |> List.map (fun s ->
                            match s.Split ":" with
                            | [| cond; wf; |] ->
                                match String.exists (fun c -> c = '<') cond with
                                | true ->
                                    match cond.Split '<' with
                                    | [| a; b; |] -> (a, Lt, (b |> int), wf)
                                    | _ -> failwith "Unexpected workflow definition"
                                | false -> 
                                    match cond.Split '>' with
                                    | [| a; b; |] -> (a, Gt, (b |> int), wf)
                                    | _ -> failwith "Unexpected workflow definition"
                            | _ -> (s,None,-1,s)
                        )
                    acc |> Map.add k v
                | _ -> failwith "Unexpected workflow definition"
            ) Map.empty
        let parts =
            tmp
            |> Array.last
            |> fun s -> s.Split "\n"
            |> Array.fold (fun acc p ->
                let part =
                    p
                    |> fun s -> s.TrimStart('{').TrimEnd('}').Split ","
                    |> Array.fold (fun fields s ->
                        match s.Split "=" with
                        | [| k; v; |] -> fields |> Map.add k (v |> int)
                        | _ -> failwith "Unexpected part definition"
                    ) Map.empty
                acc @ [part]
            ) []
        (workflows, parts)
    
    let eval rule part =
        let lhs, op, rhs, wf = rule
        match op with
        | None -> (true, wf)
        | Gt when (Map.find lhs part) > rhs -> (true, wf)
        | Lt when (Map.find lhs part) < rhs -> (true, wf)
        | _ -> (false, "")

    let score part =
        Map.find "x" part + Map.find "m" part + Map.find "a" part + Map.find "s" part

    let rate workflows part =
        let rec inner workflow =
            match workflow with
            | h::t ->
                match eval h part with
                | true, workflow ->
                    match workflow with
                    | "R" -> 0
                    | "A" -> score part
                    | _ -> inner (Map.find workflow workflows)
                | false, _ -> inner t
            | _ -> failwith "Uh oh"
        let start = Map.find "in" workflows
        inner start

    let part1 (str : string) =
        let workflows, parts = parseInput str
        parts |> List.fold (fun acc part -> acc + rate workflows part) 0

    let part2 (str : string) =
        -1