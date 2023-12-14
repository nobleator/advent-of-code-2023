module Day13
    let parseInput (str : string) =
        str.Split "\n\n"
        |> Array.map (fun grid ->
            grid.Split "\n"
            |> Array.map (fun row ->
                row |> Seq.toList
            )
            |> array2D
        )
    
    let findVerticalMirrorWithExclusion grid excludeIdx =
        let xOffsetOpt =
            [0..(grid |> Array2D.length2) - 2]
            |> List.filter (fun x -> x <> excludeIdx && grid[*,x] = grid[*,x+1])
            |> List.filter (fun xOffset ->
                [xOffset.. -1 ..0]
                |> List.forall (fun x ->
                    let rhsIdx = xOffset+1+(xOffset-x)
                    let gridWidth = (grid |> Array2D.length2)
                    rhsIdx >= gridWidth || grid[*,x] = grid[*,rhsIdx]
                )
            )
            |> List.tryExactlyOne
        match xOffsetOpt with
            | Some xOffset -> xOffset + 1
            | None -> 0

    let findVerticalMirror grid =
        findVerticalMirrorWithExclusion grid -1

    let findHorizontalMirrorWithExclusion grid excludeIdx =
        let yOffsetOpt =
            [0..(grid |> Array2D.length1) - 2]
            |> List.filter (fun y -> y <> excludeIdx && grid[y,*] = grid[y+1,*])
            |> List.filter (fun yOffset ->
                [yOffset.. -1 ..0]
                |> List.forall (fun y ->
                    let rhsIdx = yOffset+1+(yOffset-y)
                    let gridHeight = (grid |> Array2D.length1)
                    rhsIdx >= gridHeight || grid[y,*] = grid[rhsIdx,*]
                )
            )
            |> List.tryExactlyOne
        match yOffsetOpt with
            | Some yOffset -> yOffset + 1
            | None -> 0

    let findHorizontalMirror grid =
        findHorizontalMirrorWithExclusion grid -1

    let rec replaceAndFindMirrors x y grid =
        let newGrid = Array2D.init (grid |> Array2D.length1) (grid |> Array2D.length2) (fun r c ->
            match x = c && y = r with
            | false -> grid[r,c]
            | true when grid[r,c] = '.' -> '#'
            | true when grid[r,c] = '#' -> '.'
            | _ -> failwith "Cells should always be either . or #"
        )
        let xOffsetOld = findVerticalMirror grid
        let xOffsetNew = findVerticalMirrorWithExclusion newGrid (xOffsetOld - 1)
        match xOffsetOld <> xOffsetNew && xOffsetNew > 0 with
        | true -> (xOffsetNew, 0)
        | false ->
            let yOffsetOld = findHorizontalMirror grid
            let yOffsetNew = findHorizontalMirrorWithExclusion newGrid (yOffsetOld - 1)
            match yOffsetOld <> yOffsetNew && yOffsetNew > 0 with
            | true -> (0, yOffsetNew)
            | false ->
                match x < (grid |> Array2D.length2) - 1 with
                | true -> replaceAndFindMirrors (x + 1) y grid
                | false ->
                    match y < (grid |> Array2D.length1) - 1 with
                    | true -> replaceAndFindMirrors 0 (y + 1) grid
                    | false -> failwith "At least 1 cell should be replaceable"

    let part1 (str : string) =
        parseInput str
        |> Array.fold (fun acc grid ->
            let xOffset = findVerticalMirror grid
            let yOffset = findHorizontalMirror grid
            acc + xOffset + (yOffset * 100)
        ) 0
    let part2 (str : string) =
        parseInput str
        |> Array.fold (fun acc grid ->
            let xOffset, yOffset = replaceAndFindMirrors 0 0 grid
            acc + xOffset + (yOffset * 100)
        ) 0