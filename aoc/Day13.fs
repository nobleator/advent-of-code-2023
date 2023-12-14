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

    let findVerticalMirror grid =
        let xOffsetOpt =
            [0..(grid |> Array2D.length2) - 2]
            |> List.filter (fun x -> grid[*,x] = grid[*,x+1])
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

    let findHorizontalMirror grid =
        let yOffsetOpt =
            [0..(grid |> Array2D.length1) - 2]
            |> List.filter (fun y -> grid[y,*] = grid[y+1,*])
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

    let part1 (str : string) =
        parseInput str
        |> Array.fold (fun acc grid ->
            let xOffset = findVerticalMirror grid
            let yOffset = findHorizontalMirror grid
            acc + xOffset + (yOffset * 100)
        ) 0

    let part2 (str : string, expansionFactor : int) =
        -1