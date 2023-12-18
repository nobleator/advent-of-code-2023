module Day17
    open Day03
    open System.Collections.Generic

    type Node = { point: Point; direction: int * int; length: int; }
    type Node' = { Node: Node; HeatLoss: int; }

    let rec foo grid goal seen (pq : PriorityQueue<Node', int>) =
        let getHeatLoss point =
            Array2D.get grid point.y point.x
        let gridHeight = Array2D.length1 grid
        let gridWidth = Array2D.length2 grid
        match pq.TryDequeue() with
        | (false, _, _) -> failwith "No solution found"
        | (true, node, _) -> 
            let p = node.Node.point
            let l = node.Node.length
            let dr, dc = node.Node.direction
            match p = goal with
            | true -> node.HeatLoss
            | false ->
                match Set.contains node.Node seen with
                | true -> foo grid goal seen pq
                | false -> 
                    let seen' = Set.add node.Node seen
                    let straight =
                        match (p.x + dc, p.y + dr) with
                        | (nc, nr) when (dr, dc) <> (0, 0) && l < 3 &&
                            0 <= nc && nc < gridWidth &&
                            0 <= nr && nr < gridHeight ->
                            [{ Node={ point={ x=nc; y=nr; }; direction=(dr, dc); length=l + 1; }; HeatLoss=(node.HeatLoss + getHeatLoss { x=nc; y=nr; }); }]
                        | _ -> []
                    let turns =
                        [(1,0); (0,1); (-1,0); (0,-1)]
                        |> List.collect (fun (ndr, ndc) ->
                            match (p.x + ndc, p.y + ndr) with
                            | (nc, nr) when (ndr, ndc) <> (dr, dc) && (ndr, ndc) <> (-dr, -dc) &&
                                0 <= nc && nc < gridWidth &&
                                0 <= nr && nr < gridHeight ->
                                [{ Node={ point={ x=nc; y=nr; }; direction=(ndr, ndc); length=1; }; HeatLoss=(node.HeatLoss + getHeatLoss { x=nc; y=nr; }); }]
                            | _ -> []
                        )
                    straight @ turns
                    |> List.map (fun n -> pq.Enqueue(n, n.HeatLoss))
                    |> ignore
                    foo grid goal seen' pq

    let parseInput (str : string) =
        str.Split "\n"
        |> Array.map (fun row -> row |> Seq.toList |> List.map string |> List.map int)
        |> array2D
    
    let part1 (str : string) =
        let grid = parseInput str
        let gridHeight = Array2D.length1 grid
        let gridWidth = Array2D.length2 grid
        let start = { Node={ point={ x=0; y=0; }; direction=(0, 0); length=0; }; HeatLoss=0; }
        let goal = { x=gridWidth-1; y=gridHeight-1; }
        let seen = Set.empty
        let queue = new PriorityQueue<Node', int>()
        queue.Enqueue(start, 0)
        foo grid goal seen queue

    let part2 (str : string) =
        -1