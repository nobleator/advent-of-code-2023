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
                                    | [| a; b; |] -> (a, Lt, (b |> int64), wf)
                                    | _ -> failwith "Unexpected workflow definition"
                                | false -> 
                                    match cond.Split '>' with
                                    | [| a; b; |] -> (a, Gt, (b |> int64), wf)
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
                        | [| k; v; |] -> fields |> Map.add k (v |> int64)
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
                    | "R" -> 0L
                    | "A" -> score part
                    | _ -> inner (Map.find workflow workflows)
                | false, _ -> inner t
            | _ -> failwith "Uh oh"
        let start = Map.find "in" workflows
        inner start

    let scoreRange range =
        let x, m, a, s = range
        ((x |> snd) - (x |> fst) + 1L) * ((m |> snd) - (m |> fst) + 1L) * ((a |> snd) - (a |> fst) + 1L) * ((s |> snd) - (s |> fst) + 1L)

    let rateRanges workflows =
        let rec inner wf range =
            let x, m, a, s = range
            match wf with
            | h::t ->
                let lhs, op, rhs, wf = h
                match op with
                | None when wf = "R" -> 0L
                | None when wf = "A" -> scoreRange range
                | None -> inner (Map.find wf workflows) range
                | Gt when lhs = "x" -> inner t (((x |> fst), rhs), m, a, s) + inner (Map.find wf workflows) ((rhs+1L, (x |> snd)), m, a, s)
                | Lt when lhs = "x" -> inner (Map.find wf workflows) (((x |> fst), rhs-1L), m, a, s) + inner t ((rhs, (x |> snd)), m, a, s)
                | Gt when lhs = "m" -> inner t (x, ((m |> fst), rhs), a, s) + inner (Map.find wf workflows) (x, (rhs+1L, (m |> snd)), a, s)
                | Lt when lhs = "m" -> inner (Map.find wf workflows) (x, ((m |> fst), rhs-1L), a, s) + inner t (x, (rhs, (m |> snd)), a, s)
                | Gt when lhs = "a" -> inner t (x, m, ((a |> fst), rhs), s) + inner (Map.find wf workflows) (x, m, (rhs+1L, (a |> snd)), s)
                | Lt when lhs = "a" -> inner (Map.find wf workflows) (x, m, ((a |> fst), rhs-1L), s) + inner t (x, m, (rhs, (a |> snd)), s)
                | Gt when lhs = "s" -> inner t (x, m, a, ((s |> fst), rhs)) + inner (Map.find wf workflows) (x, m, a, (rhs+1L, (s |> snd)))
                | Lt when lhs = "s" -> inner (Map.find wf workflows) (x, m, a, ((s |> fst), rhs-1L)) + inner t (x, m, a, (rhs, (s |> snd)))
                | _ -> failwith "Insufficient Christmas cheer"
            | _ -> failwith "Insufficient Christmas cheer"
        let start = Map.find "in" workflows
        inner start ((1L,4000L),(1L,4000L),(1L,4000L),(1L,4000L))

    let part1 (str : string) =
        let workflows, parts = parseInput str
        parts |> List.fold (fun acc part -> acc + rate workflows part) 0L

    let part2 (str : string) =
        let workflows, _ = parseInput str
        rateRanges (workflows |> Map.add "A" [("A",None,-1,"A")] |> Map.add "R" [("R",None,-1,"R")])