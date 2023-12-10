module Day10
    open Day03

    let parseInput (str : string) =
        str.Split "\n"
        |> Array.map Seq.toList
        |> Array.mapi (fun yIdx r -> (yIdx, r))
        |> Array.fold (fun rowAcc row ->
            let yIdx, r = row
            let tmp =
                r
                |> List.mapi (fun xIdx c -> (xIdx, c))
                |> List.fold (fun colAcc col ->
                    let xIdx, c = col
                    colAcc @ [({x=xIdx; y=yIdx}, c)]
                ) []
            rowAcc @ tmp
        ) []

    let connected pair1 pair2 =
        let p1, c1 = pair1
        let p2, c2 = pair2
        let connected = (
            (
                p1.y - p2.y = 0
                &&
                (
                    (
                    p1.x - p2.x = -1
                    && String.exists (fun c -> c = c1) "-LFS"
                    && String.exists (fun c -> c = c2) "-7JS"
                    )
                    ||
                    (
                    p1.x - p2.x = 1
                    && String.exists (fun c -> c = c1) "-7JS"
                    && String.exists (fun c -> c = c2) "-LFS"
                    )
                )
            )
            ||
            (
                p1.x - p2.x = 0
                &&
                (
                    (
                    p1.y - p2.y = -1
                    && String.exists (fun c -> c = c1) "|7FS"
                    && String.exists (fun c -> c = c2) "|LJS"
                    )
                    ||
                    (
                    p1.y - p2.y = 1
                    && String.exists (fun c -> c = c1) "|LJS"
                    && String.exists (fun c -> c = c2) "|7FS"
                    )
                )
            )
            )
        connected

    let rec traverse depth lastNode currNode graph =
        let nextNode = graph |> List.find (fun x -> x <> lastNode && x <> currNode && connected currNode x)
        match nextNode |> snd with
        | 'S' -> (depth + 1) / 2
        | _ -> traverse (depth + 1) currNode nextNode graph

    let part1 (str : string) =
        let m = parseInput str
        let start = m |> List.find (fun x -> x |> snd |> (=) 'S')
        traverse 0 start start m

    // let part2 (str : string) =
    //     0