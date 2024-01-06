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
                | [] -> [path]
                | [_;] -> inner (path @ [neighbors.Head])
                | [_;_;] | [_;_;_;] -> neighbors |> List.fold (fun acc np -> acc @ inner (path @ [np])) []
                | _ -> failwith "Max of 3 neighbors expected"
        inner [start]
    
    let part1 (str : string) =
        let grid = Day16.parseInput str
        let gridHeight = (grid |> Array2D.length1)
        let gridWidth = (grid |> Array2D.length2)
        let start = { x=1; y=0; }
        let goal = { x=gridWidth-1; y=gridHeight; }
        (getRoutes grid start goal |> List.map List.length |> List.max) - 1

    let part2 (str : string) =
        -1