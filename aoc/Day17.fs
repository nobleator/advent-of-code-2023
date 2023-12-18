module Day17
    open Day03
    open Day11
    open Day14
    open Day16
    open System.Collections.Generic

    type Node = { point: Point; direction: Direction; Length: int; }

    let rec reconstruct acc node cameFrom maxLength =
        match maxLength < 0 || List.length acc < maxLength with
        | true -> 
            match Map.tryFind node cameFrom with
            | None -> node :: acc
            | Some parent -> reconstruct (node :: acc) parent cameFrom maxLength
        | false -> acc
    
    let getNeighbors node grid =
        let gridHeight = Array2D.length1 grid-1
        let gridWidth = Array2D.length2 grid-1
        let rowWise =
            match node.y with
            | y when y = 0 -> [ { x=node.x; y=node.y+1; } ]
            | y when y >= gridHeight -> [ { x=node.x; y=node.y-1; } ]
            | y when y > 0 && y < gridHeight -> [ { x=node.x; y=node.y-1; }; { x=node.x; y=node.y+1; } ]
            | _ -> failwith "Unexpected node y value"
        let colWise =
            match node.x with
            | x when x = 0 -> [ { x=node.x+1; y=node.y; } ]
            | x when x >= gridWidth -> [ { x=node.x-1; y=node.y; } ]
            | x when x > 0 && x < gridWidth -> [ { x=node.x-1; y=node.y; }; { x=node.x+1; y=node.y; } ]
            | _ -> failwith "Unexpected node x value"    
        rowWise @ colWise

    let heuristic node goal cameFrom =
        // let last3 = reconstruct [] node cameFrom 3
        // let sameRow = last3 |> List.map (fun p -> p.y) |> List.distinct |> List.length = 1
        // let sameCol = last3 |> List.map (fun p -> p.x) |> List.distinct |> List.length = 1
        // match List.length last3 = 3 && (sameRow || sameCol) with
        // | true -> distance (node, goal) + 1_000_000_000
        // | false -> distance (node, goal)
        distance (node, goal)

    let g grid node =
        getByPoint grid node |> string |> int
    
    let addOrUpdate k v m =
        match Map.containsKey k m with
        | false -> Map.add k v m
        | true ->
            Map.change k (fun x ->
                match x with
                | Some _ -> Some (v)
                | None -> None
            ) m
    
    let rec aStar start goal grid  = 
        let rec loop seen (openSet : PriorityQueue<Point, int>) cameFrom fScore gScore  = 
            let node = openSet.Dequeue()
            match node = goal with
            | true -> reconstruct [] node cameFrom -1
            | false ->
                let newSeen, newOpenSet, newCameFrom, newGScore, newFScore =
                    getNeighbors node grid
                    |> List.filter (fun n -> not (Set.contains n seen))
                    |> List.fold (fun acc neighbor ->
                        let seen', (openSet' : PriorityQueue<Point, int>), cameFrom', fScore', gScore' = acc
                        let seen'' = Set.add neighbor seen'
                        let tentative = (gScore' |> Map.find node) + g grid neighbor
                        let oldScore =
                            match gScore' |> Map.tryFind neighbor with
                            | Some x -> x
                            | None -> System.Int32.MaxValue
                        match tentative < oldScore with
                        | true ->
                            let cameFrom'' = addOrUpdate neighbor node cameFrom'
                            let gScore'' = addOrUpdate neighbor tentative gScore'
                            let f = (tentative + (heuristic neighbor goal cameFrom''))
                            printfn $"score for {node} to {neighbor} = {f}"
                            let fScore'' = addOrUpdate neighbor f fScore'
                            openSet'.Enqueue(neighbor, f)
                            (seen'', openSet', cameFrom'', fScore'', gScore'')
                        | false -> (seen'', openSet', cameFrom', fScore', gScore')
                    ) (seen, openSet, cameFrom, fScore, gScore)
                loop newSeen newOpenSet newCameFrom newFScore newGScore
        let seen = Set.empty
        let cameFrom = Map.empty
        let fScore = Map.add start (heuristic start goal cameFrom) Map.empty
        let gScore = Map.add start 0 Map.empty
        let openSet = new PriorityQueue<Point, int>()
        openSet.Enqueue(start, 1)
        loop seen openSet cameFrom fScore gScore
    
    let parseInput (str : string) =
        str.Split "\n"
        |> Array.map (fun row -> row |> Seq.toList)
        |> array2D
    
    let part1 (str : string) =
        let grid = parseInput str
        let gridHeight = Array2D.length1 grid
        let gridWidth = Array2D.length2 grid
        // let start = { x=0; y=0; }
        // let goal = { x=gridWidth-1; y=gridHeight-1; }
        let start = { x=0; y=2; }
        let goal = { x=3; y=2; }
        let path = aStar start goal grid
        printfn "optimal path: %A" path
        path
        |> List.tail
        |> List.map (fun x -> g grid x)
        |> List.sum

    let part2 (str : string) =
        -1