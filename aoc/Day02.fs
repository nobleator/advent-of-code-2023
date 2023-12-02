module Day02
    [<Literal>]
    let maxRed = 12
   
    [<Literal>]
    let maxGreen = 13
   
    [<Literal>]
    let maxBlue = 14
   
    let isRoundPossible (round : string array) =
        Array.forall (fun (x : string) ->
            x.Trim()
            |> fun x -> x.Split ' '
            |> Array.toList
            |> fun x ->
                match x with
                | count::color ->
                    match color |> List.head with
                    | "red" -> count |> int <= maxRed
                    | "green" -> count |> int <= maxGreen 
                    | "blue" -> count |> int <= maxBlue
                    | _ -> failwith "Unsupported color"
                | [] -> failwith "Empty list not expected"    
        ) round

    let isGamePossible (str : string) =
        str.Split "Game "
        |> Array.tail
        |> Array.head
        |> fun x -> x.Split ':'
        |> Array.toList
        |> fun x ->
            match x with
            | h::t ->
                if t.Head
                    |> fun y -> y.Split ';'
                    |> Array.map (fun y -> y.Split ',')
                    |> Array.forall isRoundPossible
                then h.Trim() |> int
                else 0
            | [] -> failwith "Empty list should not happen"

    let evaluate (x : string) =
        x.Split [|'\n'|]
        |> Array.toList
        |> List.fold (fun total n -> total + isGamePossible n) 0
