module Day16
    open Day03
    open Day14

    let parseInput (str : string) =
        str.Split "\n"
        |> Array.map (fun row -> row |> Seq.toList)
        |> array2D
    
    let getByPoint grid p =
        Array2D.get grid p.y p.x

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
        match Set.contains vector seen with
        | true -> seen
        | false -> traverse vector grid (Set.add vector seen)

    let part1 (str : string) =
        let grid = parseInput str
        traverse ({x=(-1); y=0}, East) grid Set.empty |> Set.map fst |> Set.count

    let part2 (str : string) =
        -1