module Day02
    [<Literal>]
    let maxRed = 12
   
    [<Literal>]
    let maxGreen = 13
   
    [<Literal>]
    let maxBlue = 14

    let isGamePossible gameMap =
        gameMap
        |> Array.forall (
            Map.forall (fun k v ->
                match k with
                | "red" -> v <= maxRed
                | "green" -> v <= maxGreen 
                | "blue" -> v <= maxBlue
                | _ -> failwith "Unsupported color"
                )
        )

    let getCubeSetPower (gameMap : Map<string, int> array) =
        gameMap
        |> Array.fold (fun acc x ->
            let r1, g1, b1 = acc
            let r2 =
                match Map.tryFind "red" x with
                | Some z -> z
                | None -> 0
            let g2 =
                match Map.tryFind "green" x with
                | Some z -> z
                | None -> 0
            let b2 =
                match Map.tryFind "blue" x with
                | Some z -> z
                | None -> 0
            (max r1 r2, max g1 g2, max b1 b2)
        ) (0, 0, 0)
        |> fun x ->
            let (a, b, c) = x
            a * b * c 

    let buildRoundMap (round : string array) =
        Array.map (fun (x : string) ->
            x.Trim()
            |> fun x -> x.Split ' '
            |> Array.toList
            |> fun tmp -> (tmp.Tail.Head, tmp.Head |> int) 
        ) round
        |> Map.ofArray

    let buildGameMap (str : string) =
        str.Split "Game "
        |> Array.tail
        |> Array.head
        |> fun x -> x.Split ':'
        |> Array.toList
        |> fun x ->
            match x with
            | h::t ->
                t.Head
                |> fun y -> y.Split ';'
                |> Array.map (fun y -> y.Split ',')
                |> Array.map buildRoundMap
                |> fun y -> (h |> int, y)
            | [] -> failwith "Empty list should not happen"

    let parseInput (x : string) =
        x.Split [|'\n'|]
        |> Array.toList
        |> List.map (fun n -> buildGameMap n)
        |> Map.ofList

    let part1 (x : string) =
        parseInput x
        |> Map.fold (fun total k v -> total + (if isGamePossible v then k else 0)) 0

    let part2 (x : string) =
        parseInput x
        |> Map.fold (fun total k v -> total + getCubeSetPower v) 0