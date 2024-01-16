module Day25
    let addNewOrAddToSet k v m =
        match Map.containsKey k m with
        | false -> Map.add k (Set.singleton v) m
        | true ->
            let tmp = Map.find k m
            Map.change k (fun x ->
                match x with
                | Some _ -> Some (Set.add v tmp)
                | None -> None
            ) m

    let addOrAppend k v m =
        match Map.containsKey k m with
        | false -> Map.add k [v] m
        | true ->
            let tmp = Map.find k m
            Map.change k (fun x ->
                match x with
                | Some _ -> Some (tmp @ [v])
                | None -> None
            ) m
    
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
        
        // adjacency list version
        let groups =
            components.Keys
            |> Seq.map (fun k -> (k, Set.singleton k))
            |> Map.ofSeq
        let ltr =
            str.Split "\n"
            |> Array.map (fun row ->
                match row.Split ":" |> Array.toList with
                | h::t ->
                    // let i = Map.find h components
                    let rhs =
                        t.Head.Trim().Split " "
                        // |> Array.map (fun x -> Map.find x components)
                        |> Array.toList
                    (h.Trim(), rhs)
                | _ -> failwith "Expecting 2 sides to colon in input"
            )
            |> Map.ofArray
        let rtl =
            ltr
            |> Map.toList
            |> List.collect (fun (k, v) ->
                v
                |> List.map (fun x ->
                    (x, k)
                )
            )
        let adjList =
            List.fold (fun acc (k, v) ->
                let v' =
                    match Map.tryFind k acc with
                    | None -> [v]
                    | Some (x) -> x @ [v] |> List.distinct
                Day20.addOrUpdate k v' acc
            ) ltr rtl

        // adjacency matrix version
        // let groups =
        //     components.Values
        //     |> Seq.map (fun k -> (k, 1))
        //     |> Map.ofSeq
        // let graph = Array2D.create components.Count components.Count 0
        // str.Split "\n"
        // |> Array.map (fun row ->
        //     match row.Split ":" |> Array.toList with
        //     | h::t ->
        //         let i = Map.find h components
        //         t.Head.Trim().Split " "
        //         |> Array.map (fun x ->
        //             let j = Map.find x components
        //             Array2D.set graph i j 1
        //             Array2D.set graph j i 1
        //         )
        //     | _ -> failwith "Expecting 2 sides to colon in input"
        // )
        // |> ignore
        (components, groups, adjList)

    let shuffle xs = xs |> Seq.sortBy (fun _ -> System.Guid.NewGuid())

    let adjMatrixSolution g gr = 
        let contractEdges i j (graph : int array2d) groups =
            // edge contraction using an adjacency matrix is slow on real input
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
            // Day14.printDish graph
            match cuts = 3 with
            | true -> product
            | false -> findTripleCut originalGraph groups

        findTripleCut g gr

    let adjListSolution g gr = 
        let midwayPoint = (Map.count g) / 2
        let contractEdges i j graph groups =
            (*
                edge contraction using an adjacency list
                1. check if only 2 nodes remain, if so, return ???
                2. pick 2 keys
                3. union the set of edges from both keys, excepting self-referential edges, and update the value for one key
                4. remove the other key from the map 
                
                starting adjacency list:
                1: 2 3 4
                2: 1 4 5
                3: 1 4
                4: 1 2 3 5
                5: 2 4

                contraction of 1 & 3:
                1+3: 2 3 4 + 1 4 - self-referential (including new identity as super-node) -> 2 4 4
                2: 1 4 5
                3: 1 4      -> deleted
                4: 1 2 3 5
                5: 2 4

                1+3: 2 4 4
                2: 1 4 5
                4: 1 2 3 5
                5: 2 4

                contraction of 2 & 4:
                1+3: 2 4 4
                2+4: 1 5 1 3 5
                5: 2 4

                contraction of 1 (a prior contraction of 1 & 3) & 2 (prior contraction of 2 & 4):
                1+3+2+4: 5 5
                5: 2 4

                edge contraction complete, min cut = longest remaining set
                if cut count <> 3 then something went wrong
            *)
            let superNode = Set.union (Map.find i groups) (Map.find j groups)
            let groups' =
                groups
                |> Day20.addOrUpdate i superNode
                |> Map.remove j
            let lhs = Map.find i graph
            let rhs = Map.find j graph
            let newLhs = lhs @ rhs |> List.filter (fun x -> not (Set.contains x superNode))
            let graph' =
                graph
                |> Day20.addOrUpdate i newLhs
                |> Map.remove j
            (graph', groups')

        let rec findCuts graph groups  =
            match groups |> Map.keys |> shuffle |> Seq.toList |> List.take 2 with
            | [ i; j; ] ->
                // printfn $"contracting {i} and {j}"
                let graph', groups' = contractEdges i j graph groups
                match groups' |> Map.count > 2 with
                | true ->
                    match Map.count graph' = midwayPoint with
                    | true -> min (findCuts graph' groups') (findCuts graph' groups')
                    | false -> findCuts graph' groups'
                | false ->
                    match groups' |> Map.keys |> Seq.toList with
                    | [a;b;] ->
                        let cuts =
                            graph'
                            |> Map.values
                            |> Seq.map (fun x -> List.length x)
                            |> Seq.max
                        let product = Map.fold (fun acc k v -> acc*(Set.count v)) 1 groups'
                        (cuts, product)
                    | _ -> failwith "Expecting pair of indices only"
            | _ -> failwith "Expecting exactly 2 indices"

        let rec findTripleCut graph groups =
            let cuts, product = findCuts graph groups
            printfn $"min cut found: {cuts}"
            match cuts = 3 with
            | true -> product
            | false -> findTripleCut graph groups

        findTripleCut g gr

    let part1 (str : string) =
        let components, groups, adjList = parseInput str
        // adjMatrixSolution graph groups
        adjListSolution adjList groups

    let part2 (str : string) =
        -1