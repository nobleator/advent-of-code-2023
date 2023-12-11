module Day11
    open Day03

    let parseInput (str : string) =
        let mat =
            str.Split "\n"
            |> Array.map Seq.toArray
            |> array2D
        str.Split "\n"
        |> Array.map Seq.toList
        |> Array.mapi (fun yIdx r -> (yIdx, r))
        |> Array.fold (fun rowAcc row ->
            let points = rowAcc
            let yIdx, r = row
            let tmp =
                r
                |> List.mapi (fun xIdx c -> (xIdx, c))
                |> List.fold (fun colAcc col ->
                    let xIdx, c = col
                    match c with
                    | '#' ->
                        let yOffset =
                            [0..yIdx]
                            |> List.fold (fun acc x ->
                                match mat[x,*] |> Array.forall (fun c -> c = '.') with
                                | true -> acc + 1
                                | false -> acc
                            ) 0
                        let xOffset =
                            [0..xIdx]
                            |> List.fold (fun acc x ->
                                match mat[*,x] |> Array.forall (fun c -> c = '.') with
                                | true -> acc + 1
                                | false -> acc
                            ) 0
                        colAcc @ [{x=(xIdx + xOffset); y=(yIdx + yOffset)}]
                    | _ -> colAcc
                ) []
            points @ tmp
        ) []

    let rec combinations l =
        match l with
        | h::t -> List.allPairs [h] t @ combinations t
        | [] -> []

    let distance points =
        let p1, p2 = points
        abs(p1.x - p2.x) + abs(p1.y - p2.y)

    let part1 (str : string) =
        parseInput str
        |> List.sortBy (fun x -> x.x + x.y)
        |> combinations 
        |> List.map distance
        |> List.sum

    // let part2 (str : string) =
    //     0