module Day16
    open Day03
    open Day14

    let parseInput (str : string) =
        str.Split "\n"
        |> Array.map (fun row -> row |> Seq.toList)
        |> array2D
    
    let getByPoint grid p =
        Array2D.get grid p.y p.x

    let energize vector grid =
        let rec traverse vector grid seen =
            let p, d = vector
            let gridHeight = Array2D.length1 grid
            let gridWidth = Array2D.length2 grid
            match d with
            | North ->
                let p' = {x=p.x; y=p.y - 1}
                match p'.y < 0 with
                | true -> seen
                | false ->
                    let nextCell = getByPoint grid p'
                    match nextCell with
                    | '.' | '|' -> checkAndRec (p', North) grid seen
                    | '/' -> checkAndRec (p', East) grid seen
                    | '\\' -> checkAndRec (p', West) grid seen
                    | '-' -> checkAndRec (p', East) grid seen |> checkAndRec (p', West) grid
                    | _ -> failwith "All symbols in grid must be . / \\ - or |"
            | South ->
                let p' = {x=p.x; y=p.y + 1}
                match p'.y >= gridHeight with
                | true -> seen
                | false ->
                    let nextCell = getByPoint grid p'
                    match nextCell with
                    | '.' | '|' -> checkAndRec (p', South) grid seen
                    | '/' -> checkAndRec (p', West) grid seen
                    | '\\' -> checkAndRec (p', East) grid seen
                    | '-' -> checkAndRec (p', East) grid seen |> checkAndRec (p', West) grid
                    | _ -> failwith "All symbols in grid must be . / \\ - or |"
            | East ->
                let p' = {x=p.x + 1; y=p.y}
                match p'.x >= gridWidth with
                | true -> seen
                | false ->
                    let nextCell = getByPoint grid p'
                    match nextCell with
                    | '.' | '-' -> checkAndRec (p', East) grid seen
                    | '/' -> checkAndRec (p', North) grid seen
                    | '\\' -> checkAndRec (p', South) grid seen
                    | '|' -> checkAndRec (p', North) grid seen |> checkAndRec (p', South) grid
                    | _ -> failwith "All symbols in grid must be . / \\ - or |"
            | West ->
                let p' = {x=p.x - 1; y=p.y}
                match p'.x < 0 with
                | true -> seen
                | false ->
                    let nextCell = getByPoint grid p'
                    match nextCell with
                    | '.' | '-' -> checkAndRec (p', West) grid seen
                    | '/' -> checkAndRec (p', South) grid seen
                    | '\\' -> checkAndRec (p', North) grid seen
                    | '|' -> checkAndRec (p', North) grid seen |> checkAndRec (p', South) grid
                    | _ -> failwith "All symbols in grid must be . / \\ - or |"
        and checkAndRec vector grid seen =
            // TODO: caching each vector and its corresponding final score would be a huge improvement
            match Set.contains vector seen with
            | true -> seen
            | false -> traverse vector grid (Set.add vector seen)
        traverse vector grid Set.empty |> Set.map fst |> Set.count

    let part1 (str : string) =
        let grid = parseInput str
        energize ({x=(-1); y=0}, East) grid

    let part2 (str : string) =
        let grid = parseInput str
        let left =
            [|0..(Array2D.length1 grid-1)|]
            |> Array.map (fun y -> energize ({x=(-1); y=y}, East) grid)
            |> Array.max
        let right =
            [|0..(Array2D.length1 grid-1)|]
            |> Array.map (fun y -> energize ({x=(Array2D.length2 grid-1); y=y}, West) grid)
            |> Array.max
        let top =
            [|0..(Array2D.length2 grid-1)|]
            |> Array.map (fun x -> energize ({x=x; y=(-1)}, South) grid)
            |> Array.max
        let bottom =
            [|0..(Array2D.length2 grid-1)|]
            |> Array.map (fun x -> energize ({x=x; y=(Array2D.length1 grid-1)}, North) grid)
            |> Array.max
        List.max [left; right; top; bottom]