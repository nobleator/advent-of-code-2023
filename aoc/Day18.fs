module Day18
    let determinant coordPair =
        let a, b = coordPair
        ((a |> fst) * (b |> snd)) - ((a |> snd) * (b |> fst))
    
    let shoelace points =
        points @ [points.Head]
        |> List.pairwise
        |> List.map determinant
        |> List.sum
        |> float
        |> fun x -> x / 2.0
    
    let distance coordPair =
        let a, b = coordPair
        abs((a |> fst) - (b |> fst)) + abs((a |> snd) - (b |> snd))

    let perimeter points =
        points @ [points.Head]
        |> List.pairwise
        |> List.map distance
        |> List.sum
        |> float

    let movesToCoords moves =
        moves
        |> List.fold (fun acc m ->
            let d, r, _ = m
            let x, y = acc |> List.head
            match d with
            | "R" -> [(x+r,y)] @ acc
            | "L" -> [(x-r,y)] @ acc
            | "U" -> [(x,y-r)] @ acc
            | "D" -> [(x,y+r)] @ acc
            | _ -> failwith "Direction not supported"
        ) [(0,0)]

    let parseInput (str : string) =
        str.Split "\n"
        |> Array.toList
        |> List.map (fun r ->
            match r.Split " " with
            | [| dir; rng; hex |] -> (dir, rng |> int, hex)
            | _ -> failwith "The number of elements in a row is expected to be 3"
        )

    let part1 (str : string) =
        let coords = parseInput str |> movesToCoords
        let interior = coords |> shoelace
        let perimeter = coords |> perimeter
        abs(interior) + (perimeter / 2.0) + 1.0

    let part2 (str : string) =
        -1