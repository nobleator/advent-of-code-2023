module Day03
    type Point = { x: int; y: int}

    let adjacent p1 p2 =
        abs(p1.x - p2.x) <= 1 && abs(p1.y - p2.y) <= 1
    
    let chunkBy cond ls =
        (ls, [])
        ||> List.foldBack (fun l s ->
            match s with 
            | [] | [[]] -> [[l]]
            | (n::res)::ns ->
                if cond l n
                then (l::n::res)::ns
                else [l]::(n::res)::ns)
    
    let consolidateNumbers numList =
        numList
        |> chunkBy (fun prev next ->
            let p1, _ = prev
            let p2, _ = next
            abs (p1.x - p2.x) <= 1
        )

    let convertToInt numList =
        numList
        |> List.map (fun x ->
            let num =
                x
                |> List.sortBy (fun y -> 
                    (y |> fst).x
                )
                |> List.fold (fun acc y ->
                    let c = y |> snd
                    acc + c
                ) ""
                |> int
            let pointList =
                x
                |> List.map (fun x ->
                    let p, _ = x
                    p
                )
            (pointList, num)
        )

    let parseInput (x : string) =
        let chars =
            x.Split [|'\n'|]
            |> Array.map Seq.toList
        let symbols =
            chars
            |> Array.mapi (fun yIdx r -> (yIdx, r))
            |> Array.fold (fun rowAcc row ->
                let yIdx, r = row
                let tmp =
                    r
                    |> List.mapi (fun xIdx c -> (xIdx, c))
                    |> List.fold (fun colAcc col ->
                        let xIdx, c = col
                        match c with
                        | '*' | '#' | '+' | '$' | '@' |'/' | '&' | '%' | '-' | '=' -> colAcc @ [{x=xIdx; y=yIdx}]
                        | _ -> colAcc
                    ) []
                rowAcc @ tmp
            ) []
        let numbers =
            chars
            |> Array.mapi (fun yIdx r -> (yIdx, r))
            |> Array.fold (fun rowAcc row ->
                let yIdx, r = row
                let tmp =
                    r
                    |> List.mapi (fun xIdx c -> (xIdx, c))
                    |> List.fold (fun colAcc col ->
                        let xIdx, c = col
                        match System.Char.IsDigit c with
                        | true -> colAcc @ [({x=xIdx; y=yIdx}, c.ToString())]
                        | _ -> colAcc
                    ) []
                rowAcc @ tmp
            ) []
            |> consolidateNumbers
            |> convertToInt
        (symbols, numbers)

    let part1 (x : string) =
        let symbols, numbers = parseInput x
        numbers
        |> List.fold (fun acc x ->
            let adj =
                x
                |> fst
                |> List.exists (fun y ->
                    symbols
                    |> List.exists (fun z ->
                        adjacent y z
                    )
                )
            if adj then acc + (x |> snd) else acc
        ) 0

    // let part2 (x : string) =
    //     parseInput x
    //     |> Map.fold (fun total k v -> total + getCubeSetPower v) 0