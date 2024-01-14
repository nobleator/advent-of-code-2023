module Day25
    let parseInput (str : string) =
        let components =
            str.Split "\n"
            |> Array.collect (fun row ->
                row.Split " " |> Array.map (fun x -> x.TrimEnd ':')
            )
            |> Array.distinct
            |> Array.sort
            |> Array.mapi (fun i x -> (x, i))
            |> Map.ofArray
        let groups =
            components.Values
            |> Seq.map (fun k -> (k, 1))
            |> Map.ofSeq
        let graph = Array2D.create components.Count components.Count 0
        // Day14.printDish graph
        str.Split "\n"
        |> Array.map (fun row ->
            match row.Split ":" |> Array.toList with
            | h::t ->
                let i = Map.find h components
                t.Head.Trim().Split " "
                |> Array.map (fun x ->
                    let j = Map.find x components
                    Array2D.set graph i j 1
                    Array2D.set graph j i 1
                )
            | _ -> failwith "Expecting 2 sides to colon in input"
        )
        |> ignore
        (components, groups, graph)

    let shuffle xs = xs |> Seq.sortBy (fun _ -> System.Guid.NewGuid())

    let contractEdges i j (graph : int array2d) groups =
        // let graph' = Array2D.copy graph
        let lhs = graph[*,i]
        let rhs = graph[*,j]
        let newLhs =
            Array.zip lhs rhs
            |> Array.mapi (fun idx (l, r) -> [| if idx<>i then l+r else 0|])
            |> array2D
        let newTop =
            [|
                Array.zip lhs rhs
                |> Array.mapi (fun idx (l, r) -> if idx<>i then l+r else 0)
            |]
            |> array2D
        let newRhs = Array2D.create (Array2D.length1 newLhs) (Array2D.length2 newLhs) 0
        let newBottom = Array2D.create (Array2D.length1 newTop) (Array2D.length2 newTop) 0
        Array2D.blit newLhs 0 0 graph 0 i (Array2D.length1 newLhs) (Array2D.length2 newLhs)
        Array2D.blit newTop 0 0 graph i 0 (Array2D.length1 newTop) (Array2D.length2 newTop)
        Array2D.blit newRhs 0 0 graph 0 j (Array2D.length1 newRhs) (Array2D.length2 newRhs)
        Array2D.blit newBottom 0 0 graph j 0 (Array2D.length1 newBottom) (Array2D.length2 newBottom)
        let lhsCount = Map.find i groups
        let rhsCount = Map.find j groups
        let groups' =
            groups
            |> Day20.addOrUpdate i (lhsCount+rhsCount)
            |> Day20.addOrUpdate j 0
        groups'

    let rec findCuts graph groups  =
        (*
            edge contraction using an adjacency matrix
            given a starting matrix of 1 and 0, where the cell indicates the number of edges between 2 nodes
            1. check if only 2 nodes remain, if so, return ???
            2. pick 2 indices that haven't been contracted yet
            3. add the vector of one index/node to the vector of the other index/node
            4. set diagonals back to 0 (remove self-referential edges)
            
            starting adjacency matrix:
                 1  2  3  4  5
            1   [0; 1; 1; 1; 0;]
            2   [1; 0; 0; 1; 1;]
            3   [1; 0; 0; 1; 0;]
            4   [1; 1; 1; 0; 1;]
            5   [0; 1; 0; 1; 0;]

            contraction of 1 & 3:
                 1  2  3  4  5
            1   [0; 1; 0; 2; 0;]
            2   [1; 0; 0; 1; 1;]
            3   [0; 0; 0; 0; 0;]
            4   [2; 1; 0; 0; 1;]
            5   [0; 1; 0; 1; 0;]

            contraction of 2 & 4:
                 1  2  3  4  5
            1   [0; 3; 0; 0; 0;]
            2   [3; 0; 0; 0; 2;]
            3   [0; 0; 0; 0; 0;]
            4   [0; 0; 0; 0; 0;]
            5   [0; 2; 0; 0; 0;]

            contraction of 1 (a prior contraction of 1 & 3) & 2 (prior contraction of 2 & 4):
                 1  2  3  4  5
            1   [0; 0; 0; 0; 2;]
            2   [0; 0; 0; 0; 0;]
            3   [0; 0; 0; 0; 0;]
            4   [0; 0; 0; 0; 0;]
            5   [2; 0; 0; 0; 0;]

            edge contraction complete, min cut = only cell remaining in upper diagonal
            if cut count <> 3 then something went wrong
        *)
        match groups |> Map.filter (fun k v -> v>0) |> Map.keys |> shuffle |> Seq.toList |> List.take 2 with
        | [ i; j; ] ->
            // printfn $"contracting {i} and {j}"
            // graph is mutable and updated by contractEdges
            let groups' = contractEdges i j graph groups
            match Map.filter (fun k v -> v>0) groups' |> Map.count > 2 with
            | true -> findCuts graph groups'
            | false ->
                // check for 3 cuts
                match Map.filter (fun k v -> v>0) groups' |> Map.keys |> Seq.toList with
                | [a;b;] ->
                    let cuts = graph[a,b]
                    let product =
                        Map.filter (fun k v -> v>0) groups'
                        |> Map.fold (fun acc k v -> acc*v) 1
                    (cuts, product)
                | _ -> failwith "Expecting pair of indices only"
        | _ -> failwith "Expecting exactly 2 indices"

    let rec findTripleCut graph groups =
        let originalGraph = Array2D.copy graph
        let cuts, product = findCuts graph groups
        printfn $"min cut found: {cuts}"
        match cuts = 3 with
        | true -> product
        | false -> findTripleCut originalGraph groups

    let part1 (str : string) =
        // component to index map (string to count (int))
        // adjacency matrix (int array2d)
        // component consolidation count map (index (int) to count (int))
        let components, groups, graph = parseInput str
        // Day14.printDish graph
        findTripleCut graph groups
        // let g, g2 = contractEdges 0 2 graph groups
        // Day14.printDish g
        // let g, g2' = contractEdges 1 3 g g2
        // Day14.printDish g
        // let g, g2'' = contractEdges 0 1 g g2'
        // Day14.printDish g

    let part2 (str : string) =
        -1