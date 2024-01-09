module Day23
    open Day03

    type Node = { id: string; path: Point list; children: Node list; }

    let addOrAppend k v m =
        match Map.containsKey k m with
        | false -> Map.add k v m
        | true ->
            let v' = Map.find k m @ v
            Map.change k (fun x ->
                match x with
                | Some _ -> Some (v')
                | None -> None
            ) m

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

    let pointToString p =
        $"({p.x},{p.y})"

    let getGraph grid start goal =
        let gridHeight = (grid |> Array2D.length1)
        let gridWidth = (grid |> Array2D.length2)
        let rec inner path seen =
            let p = path |> List.last
            let id = $"{List.head path |> pointToString}->{pointToString p}"
            let seen' = Set.add p seen
            match List.contains goal path with
            | true -> { id=id; path=path; children=[]; }
            | false ->
                let neighbors =
                    [(0,1); (0,-1); (1,0); (-1,0);]
                    |> List.map (fun (nx, ny) -> { x=p.x+nx; y=p.y+ny; })
                    |> List.filter (fun np ->
                        let inBounds = np.x > 0 && np.x < gridWidth && np.y > 0 && np.y < gridHeight
                        let c = if inBounds then Day16.getByPoint grid np else '#'
                        inBounds && c <> '#' && not (Set.contains np seen')
                    )
                match neighbors with
                | [] -> { id=id; path=path; children=[]; }
                | [_;] -> inner (path @ [neighbors.Head]) seen'
                | [_;_;] | [_;_;_;] ->
                    let children = neighbors |> List.map (fun np -> inner [np] seen')
                    { id=id; path=path; children=children; }
                | _ -> failwith "Max of 3 neighbors expected"
        inner [start] (Set.singleton start)
    
    let getId path =
        let path' = List.sort path
        $"{List.head path' |> pointToString}->{List.last path' |> pointToString}"

    let getGraph2 grid start goal =
        let gridHeight = (grid |> Array2D.length1)
        let gridWidth = (grid |> Array2D.length2)
        let rec inner parent candidate path seen graph nodes =
            let id =
                if List.length path > 0
                then getId path
                else $"{pointToString candidate}"
            let seen' = Set.add candidate seen
            let neighbors =
                [(0,1); (0,-1); (1,0); (-1,0);]
                |> List.map (fun (nx, ny) -> { x=candidate.x+nx; y=candidate.y+ny; })
                |> List.filter (fun np ->
                    let inBounds = np.x > 0 && np.x < gridWidth && np.y > 0 && np.y < gridHeight
                    let c = if inBounds then Day16.getByPoint grid np else '#'
                    inBounds && c <> '#' && not (Set.contains np seen')
                )
            match neighbors with
            | [] ->
                let graph' = addOrAppend id [parent] graph
                let nodes' = Day20.addOrUpdate id (path @ [candidate]) nodes
                (graph', nodes')
            | [_;] -> inner parent neighbors.Head (path @ [candidate]) seen' graph nodes
            | [_;_;] | [_;_;_;] ->
                let graph' = addOrAppend id [getId path; getId [candidate];] graph
                let nodes' =
                    Day20.addOrUpdate id path nodes
                    |> Day20.addOrUpdate (getId [candidate]) [candidate]
                let (graph'', nodes'') =
                    neighbors
                    |> List.fold (fun (g, n) np ->
                        inner (getId [candidate]) np [] seen' g n
                    ) (graph', nodes')
                (graph'', nodes'')
            | _ -> failwith "Max of 3 neighbors expected"
        inner (getId [start]) start [] (Set.singleton start) Map.empty Map.empty  

    let rec getAllRoutes graph start goal =
        let rec inner q all =
            match q with
            | route::rest ->
                match List.contains goal route with
                | true -> inner rest (all @ [route])
                | false -> 
                    let q' =
                        Map.find (route |> List.last) graph
                        |> List.filter (fun x -> not (List.contains x route))
                        |> List.map (fun x -> route @ [x])
                        |> List.append rest
                    inner q' all
            | _ -> all
        inner [[start]] []

    let getNeighbors grid point seen =
        let gridHeight = (grid |> Array2D.length1)
        let gridWidth = (grid |> Array2D.length2)
        [(0,1); (0,-1); (1,0); (-1,0);]
        |> List.map (fun (nx, ny) -> { x=point.x+nx; y=point.y+ny; })
        |> List.filter (fun np ->
            if List.contains np [{x=106;y=17;}; {x=108;y=17;}; {x=107;y=16;}; {x=107;y=18;};]
            then
                printfn $"{np} being checked by {point}"
            let inBounds = np.x > 0 && np.x < gridWidth && np.y > 0 && np.y < gridHeight
            let c = if inBounds then Day16.getByPoint grid np else '#'
            let alreadySeen = Set.contains np seen
            inBounds &&
            c <> '#' &&
            not alreadySeen
        )

    let getJunctions grid start =
        let mutable junctions = Set.singleton start
        let mutable seen = Set.empty<Point>
        let rec inner queue =
            match queue with
            | candidate::rest ->
                if List.contains candidate [{x=107;y=17;}; {x=127;y=33;}; {x=127;y=61;}; {x=129;y=89;}; {x=133;y=99;};]
                then
                    printfn "missing junction"

                seen <- Set.add candidate seen
                let neighbors = getNeighbors grid candidate seen
                match neighbors with
                | [] -> junctions
                | [_;] -> inner (neighbors @ rest)
                | [_;_;] | [_;_;_;] ->
                    junctions <- Set.add candidate junctions
                    inner (neighbors @ rest)
                | _ -> failwith "Max of 3 neighbors expected"
            | _ -> junctions
        inner [start]

    let buildGraph grid junctions start =
        let mutable graph = Map.empty<Point,int>
        let mutable seen = Set.singleton start
        let rec inner queue =
            let start2 = start
            match queue with
            | h::t ->
                let (candidate, counter) = h
                // let seen' = Set.add candidate seen
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
            // printfn $"queue len = {List.length queue}"
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
        let expectedJunctions =
            List.allPairs [0..gridWidth-1] [0..gridHeight-1]
            |> List.filter (fun (x,y) -> Day16.getByPoint grid {x=x;y=y;} = 'X')
            |> List.map (fun (x,y) -> {x=x;y=y;})
            |> Set.ofList
        let junctions = getJunctions grid start |> Set.add goal
        let missing = Set.difference expectedJunctions junctions
        let graph =
            junctions
            |> Set.map (fun j -> (j, buildGraph grid junctions j))
            |> Map.ofSeq
        // let test = Map.findKey (fun k v -> Map.exists (fun k v2 -> List.contains {x=85;y=59;} v2) v) graph
        // let unexpectedJunctions =
        //     graph
        //     |> Map.filter (fun k v ->
        //         v |> Map.count > 4
        //     )
        // (bruteForce graph start goal |> List.map snd |> List.max)-1
        -1