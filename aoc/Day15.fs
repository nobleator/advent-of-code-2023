module Day15
    let parseInput (str : string) =
        str.Split ','
        |> Array.map Seq.toList
    
    let hash chars =
        chars |> List.fold (fun acc c -> ((acc + (c |> int)) * 17) % 256) 0

    let assignBoxes codes =
        codes
        |> Array.fold (fun boxes code ->
            match code |> List.contains '=' with
            | true ->
                let label, focal = code |> List.splitAt (code |> List.findIndex (fun x -> x = '='))
                let label' = label |> string
                let box = label |> hash
                let focal' = focal.Tail |> List.map string |> String.concat "" |> int
                match boxes |> Map.tryFind box with
                | Some b ->
                    boxes |> Map.change box (fun k ->
                        match k with
                        | Some v ->
                            let idxOpt = v |> List.tryFindIndex (fun x -> (x |> fst) = label')
                            match idxOpt with
                            | Some idx -> Some (v |> List.updateAt idx (label', focal'))
                            | None ->  Some (v @ [(label', focal')])
                        | None -> None
                    )
                | None -> boxes |> Map.add box [(label', focal')]
            | false ->
                let label, _ = code |> List.splitAt (code |> List.findIndex (fun x -> x = '-'))
                let label' = label |> string
                let box = label |> hash
                match boxes |> Map.tryFind box with
                | Some b ->
                    boxes |> Map.change box (fun k ->
                        match k with
                        | Some v ->
                            let idxOpt = v |> List.tryFindIndex (fun x -> (x |> fst) = label')
                            match idxOpt with
                            | Some idx -> Some (v |> List.removeAt idx)
                            | None -> Some (v)
                        | None -> None
                    )
                | None -> boxes
        ) Map.empty

    let calculatePower boxes =
        boxes
        |> Map.fold (fun acc k v -> 
            let tmp =
                v
                |> List.mapi (fun i x ->
                    let _, f = x
                    (k + 1) * (i + 1) * f
                )
                |> List.sum
            acc + tmp
        ) 0

    let part1 (str : string) =
        parseInput str |> Array.fold (fun acc x -> acc + hash x) 0

    let part2 (str : string) =
        parseInput str
        |> assignBoxes
        |> calculatePower