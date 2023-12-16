module Day16
    open Day03
    open Day14

    let parseInput (str : string) =
        str.Split "\n"
        |> Array.map (fun row -> row |> Seq.toList)
        |> array2D
    
    let getByPoint grid p =
        Array2D.get grid p.y p.x

    let mutable seen = Set.empty

    let rec traverse path grid =
        let p, d = path |> List.head
        let gridHeight = Array2D.length1 grid
        let gridWidth = Array2D.length2 grid
        match d with
        | North ->
            let p' = {x=p.x; y=p.y - 1}
            match p'.y < 0 with
            | true -> path |> List.map fst |> Set.ofList
            | false ->
                let nextCell = getByPoint grid p'
                match nextCell with
                | '.' | '|' -> checkAndRec (p', North) path grid
                | '/' -> checkAndRec (p', East) path grid
                | '\\' -> checkAndRec (p', West) path grid
                | '-' -> Set.union (checkAndRec (p', East) path grid) (checkAndRec (p', West) path grid)
                | _ -> failwith "All symbols in grid must be . / \\ - or |"
        | South ->
            let p' = {x=p.x; y=p.y + 1}
            match p'.y >= gridHeight with
            | true -> path |> List.map fst |> Set.ofList
            | false ->
                let nextCell = getByPoint grid p'
                match nextCell with
                | '.' | '|' -> checkAndRec (p', South) path grid
                | '/' -> checkAndRec (p', West) path grid
                | '\\' -> checkAndRec (p', East) path grid
                | '-' -> Set.union (checkAndRec (p', East) path grid) (checkAndRec (p', West) path grid)
                | _ -> failwith "All symbols in grid must be . / \\ - or |"
        | East ->
            let p' = {x=p.x + 1; y=p.y}
            match p'.x >= gridWidth with
            | true -> path |> List.map fst |> Set.ofList
            | false ->
                let nextCell = getByPoint grid p'
                match nextCell with
                | '.' | '-' -> checkAndRec (p', East) path grid
                | '/' -> checkAndRec (p', North) path grid
                | '\\' -> checkAndRec (p', South) path grid
                | '|' -> Set.union (checkAndRec (p', North) path grid) (checkAndRec (p', South) path grid)
                | _ -> failwith "All symbols in grid must be . / \\ - or |"
        | West ->
            let p' = {x=p.x - 1; y=p.y}
            match p'.x < 0 with
            | true -> path |> List.map fst |> Set.ofList
            | false ->
                let nextCell = getByPoint grid p'
                match nextCell with
                | '.' | '-' -> checkAndRec (p', West) path grid
                | '/' -> checkAndRec (p', South) path grid
                | '\\' -> checkAndRec (p', North) path grid
                | '|' -> Set.union (checkAndRec (p', North) path grid) (checkAndRec (p', South) path grid)
                | _ -> failwith "All symbols in grid must be . / \\ - or |"
    and checkAndRec vector path grid =
        match Set.contains vector seen with
        | true -> path |> List.map fst |> Set.ofList
        | false ->
            seen <- Set.add vector seen
            traverse ([vector] @ path) grid

    let part1 (str : string) =
        parseInput str |> traverse [({x=(-1); y=0}, East)] |> ignore
        Set.count (seen |> Set.map fst)

    let part2 (str : string) =
        -1