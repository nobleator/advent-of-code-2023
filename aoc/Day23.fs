module Day23
    open Day03

    let getRoutes grid start goal =
        let gridHeight = (grid |> Array2D.length1)
        let gridWidth = (grid |> Array2D.length2)
        let rec inner path =
            match List.contains goal path with
            | true -> [path]
            | false ->
                let p = path |> List.last
                let neighbors =
                    [(0,1); (0,-1); (1,0); (-1,0);]
                    |> List.map (fun (nx, ny) -> { x=p.x+nx; y=p.y+ny; })
                    |> List.filter (fun np ->
                        let inBounds = np.x > 0 && np.x < gridWidth && np.y > 0 && np.y < gridHeight
                        let c = if inBounds then Day16.getByPoint grid np else '#'
                        let isUphill = (np.x < p.x && c = '>') || (np.y < p.y && c = 'v')
                        inBounds && not isUphill && c <> '#' && not (List.contains np path)
                    )
                match neighbors with
                | [] -> []
                | [_;] -> inner (path @ [neighbors.Head])
                | [_;_;] | [_;_;_;] -> neighbors |> List.fold (fun acc np -> acc @ inner (path @ [np])) []
                | _ -> failwith "Max of 3 neighbors expected"
        inner [start]

    let getNeighbors grid point seen =
        let gridHeight = (grid |> Array2D.length1)
        let gridWidth = (grid |> Array2D.length2)
        [(0,1); (0,-1); (1,0); (-1,0);]
        |> List.map (fun (nx, ny) -> { x=point.x+nx; y=point.y+ny; })
        |> List.filter (fun np ->
            let inBounds = np.x > 0 && np.x < gridWidth && np.y > 0 && np.y < gridHeight
            let c = if inBounds then Day16.getByPoint grid np else '#'
            let alreadySeen = Set.contains np seen
            inBounds && c <> '#' && not alreadySeen
        )

    let getJunctions grid start =
        let mutable junctions = Set.singleton start
        let mutable seen = Set.empty<Point>
        let rec inner queue =
            match queue with
            | candidate::rest ->
                seen <- Set.add candidate seen
                let neighbors = getNeighbors grid candidate seen
                match neighbors with
                | [] -> inner rest
                | [_;] -> inner (neighbors @ rest)
                | [_;_;] | [_;_;_;] ->
                    junctions <- Set.add candidate junctions
                    inner (neighbors @ rest)
                | _ -> failwith "Max of 3 neighbors expected"
            | _ -> junctions
        inner [start]

    let getGraph grid junctions start =
        let mutable graph = Map.empty<Point,int>
        let mutable seen = Set.singleton start
        let rec inner queue =
            match queue with
            | h::t ->
                let (candidate, counter) = h
                seen <- Set.add candidate seen
                match Set.contains candidate junctions with
                | true ->
                    graph <- Day20.addOrUpdate candidate counter graph
                    inner t
                | false ->
                    let neighbors = getNeighbors grid candidate seen
                    match neighbors with
                    | [] -> inner t
                    | _ -> inner ((neighbors |> List.map (fun n -> (n, counter+1))) @ t)
            | _ -> graph
        let neighbors = getNeighbors grid start Set.empty
        inner (neighbors |> List.map (fun n -> (n, 1)))

    let bruteForce graph start goal =
        let mutable routes = List.empty<Point list * int>
        let rec inner queue =
            match queue with
            | h::t ->
                let (path, count) = h
                let node = List.last path
                match node = goal with
                | true ->
                    routes <- List.append [h] routes
                    inner t
                | false ->
                    let connectedNodes =
                        Map.find node graph
                        |> Map.toList
                        |> List.filter (fun (n, c) -> not (List.contains n path))
                        |> List.map (fun (n, c) -> (path @ [n], count+c))
                    inner (connectedNodes @ t)
            | _ -> routes
        inner [([start], 1)]

    let part1 (str : string) =
        let grid = Day16.parseInput str
        let gridHeight = (grid |> Array2D.length1)
        let gridWidth = (grid |> Array2D.length2)
        let start = { x=1; y=0; }
        let goal = { x=gridWidth-2; y=gridHeight-1; }
        (getRoutes grid start goal |> List.map List.length |> List.max) - 1

    let part2 (str : string) =
        let grid = Day16.parseInput str
        let gridHeight = (grid |> Array2D.length1)
        let gridWidth = (grid |> Array2D.length2)
        let start = { x=1; y=0; }
        let goal = { x=gridWidth-2; y=gridHeight-1; }
        let junctions = getJunctions grid start |> Set.add goal
        let graph =
            junctions
            |> Set.map (fun j -> (j, getGraph grid junctions j))
            |> Map.ofSeq
        (bruteForce graph start goal |> List.map snd |> List.max)-1