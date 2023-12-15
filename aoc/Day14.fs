module Day14
    let parseInput (str : string) =
        str.Split "\n"
        |> Array.map (fun row -> row |> Seq.toList)
        |> array2D
    
    type Direction =
    | North
    | South
    | East
    | West

    let rec slide seq =
        match seq with
        | [] -> seq
        | _ ->
            let rockIdxOpt = List.tryFindIndex (fun x -> x = '#') seq
            let rockIdx =
                match rockIdxOpt with
                | None -> List.length seq
                | Some idx -> idx
            let sliding, blocked = seq |> List.splitAt rockIdx
            let numRocks = sliding |> List.filter (fun x -> x = 'O') |> List.length
            let tmp =
                match numRocks > 0 with
                | false -> sliding
                | true -> ([0..numRocks-1] |> List.map (fun _ -> 'O')) @ ([numRocks..rockIdx-1] |> List.map (fun _ -> '.'))
            match blocked with
            | h::t -> tmp @ [h] @ slide t
            | [] -> tmp

    let printDish dish =
        [0..(Array2D.length1 dish)-1]
        |> List.map (fun x -> dish[x,*] |> Array.map string |> String.concat "")
        |> String.concat "\n"
        |> fun x -> printfn $"dish:\n\n{x}\n"

    let tilt dir dish =
        match dir with
        | North ->
            [0..(Array2D.length1 dish)-1]
            |> List.map (fun x ->
                let arr = dish[*,x]
                let newArr =
                    arr
                    |> Array.toList
                    |> slide
                    |> List.map (fun c -> [| c |])
                    |> List.toArray
                    |> array2D
                Array2D.blit newArr 0 0 dish 0 x (Array2D.length1 newArr) (Array2D.length2 newArr)
            )
            |> ignore
            dish
        | _ -> failwith "Direction not implemented"

    let getWeight dir dish =
        match dir with
        | North ->
            [0..(Array2D.length2 dish)-1]
            |> List.fold (fun acc rIdx ->
                let numRocks = (dish[rIdx,*] |> Array.filter (fun x -> x = 'O') |> Array.length)
                let dist = (Array2D.length1 dish - rIdx)
                acc + (numRocks * dist)
            ) 0
        | _ -> failwith "Direction not implemented"

    let part1 (str : string) =
        parseInput str |> tilt North |> getWeight North

    let part2 (str : string, expansionFactor : int) =
        -1