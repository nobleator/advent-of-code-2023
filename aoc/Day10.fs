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

    let exists str char = (Set.ofSeq str |> Set.contains char)

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
                    && exists "-LFS" c1
                    && exists "-7JS" c2
                    )
                    ||
                    (
                    p1.x - p2.x = 1
                    && exists "-7JS" c1
                    && exists "-LFS" c2
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
                    && exists "|7FS" c1
                    && exists "|LJS" c2
                    )
                    ||
                    (
                    p1.y - p2.y = 1
                    && exists "|LJS" c1
                    && exists "|7FS" c2
                    )
                )
            )
            )
        connected

    let rec traverse path lastNode currNode graph =
        let nextNode =
            graph
            |> List.find (fun x -> x <> lastNode && x <> currNode && connected currNode x)
        let direction = (nextNode |> fst).y - (currNode |> fst).y
        let newPath = path @ [(nextNode, direction)]
        match nextNode |> snd with
        | 'S' -> newPath
        | _ -> traverse newPath currNode nextNode graph

    let replaceStartSymbol path start =
        let point, _ = start
        let linkedToStart =
            path
            |> List.filter (fun x -> connected x start)
            |> List.map fst
        let newStartChar =
            match linkedToStart with
            | [ a; b; ] when a.y = point.y && b.y = point.y -> '-'
            | [ a; b; ] when a.x = point.x && b.x = point.x -> '|'
            | [ a; b; ] when (a.x < point.x && b.y < point.y) || (b.x < point.x && a.y < point.y) -> 'J'
            | [ a; b; ] when (a.x > point.x && b.y > point.y) || (b.x > point.x && a.y > point.y) -> 'F'
            | [ a; b; ] when (a.x < point.x && b.y > point.y) || (b.x < point.x && a.y > point.y) -> '7'
            | [ a; b; ] when (a.x > point.x && b.y < point.y) || (b.x > point.x && a.y < point.y) -> 'L'
            | _ -> failwith "There should be exactly 2 nodes linked to the S node"
        let newStart = (point, newStartChar)
        let idx =
            path
            |> List.findIndex (fun x -> x = start)
        let newPath =
            path
            |> List.updateAt idx newStart
        newPath

    let part1 (str : string) =
        let m = parseInput str
        let start = m |> List.find (fun x -> x |> snd |> (=) 'S')
        let path = traverse [(start, 0)] start start m
        (path |> List.length) / 2

    let part2 (str : string) =
        let allPoints = parseInput str
        let start = allPoints |> List.find (fun x -> x |> snd |> (=) 'S')
        let path = traverse [(start, 0)] start start allPoints
        let newPath = replaceStartSymbol (path |> List.map fst) start
        let newAllPoints = replaceStartSymbol allPoints start
        let chunked =
            newAllPoints
            |> List.chunkBySize (str.Split "\n" |> Array.map Seq.toList |> Array.head |> List.length)
        chunked
        |> List.collect (fun r ->
            let mutable isContained = false
            r |> List.map (fun c -> 
                match newPath |> List.tryFind (fun x -> (x |> fst).x = (c |> fst).x && (x |> fst).y = (c |> fst).y) with
                | Some x when exists "|7F" (x |> snd) ->
                    isContained <- not isContained
                    (c, 0)
                | Some x -> (c, 0)
                | None when isContained -> (c, 1)
                | None -> (c, 0)
            )
        )
        |> List.filter (fun x -> (x |> snd) = 1)
        |> List.map snd
        |> List.sum