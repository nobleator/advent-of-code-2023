module Day04
    let toIntArray (str : string) =
        str.Split ' '
        |> Array.filter (fun z -> not (System.String.IsNullOrWhiteSpace z))
        |> Array.map int
    
    let parseInput (x : string) =
        x.Split [|'\n'|]
        |> Array.map (fun x ->                    
            let y = x.Split ':' |> Array.tail |> Array.head
            match y.Split '|' |> Array.toList with
            | [ a ; b ] ->
                let wins = toIntArray a |> Set.ofArray
                let picked = toIntArray b |> Set.ofArray
                (wins, picked)
            | _ -> failwith "Unexpected failed split"
        )

    let score games =
        games
        |> Array.fold (fun acc x ->
            let w, p = x
            let m = Set.intersect w p
            let exp = max 0 (m.Count - 1)
            let pow = (2.0 ** exp) |> int
            let inc = (min 1 m.Count) * pow
            acc + inc
        ) 0

    let part1 (x : string) =
        parseInput x |> score