module Day22
    type Brick = { x: int * int; y: int * int; z: int * int; }
    
    let parseInput (str : string) =
        str.Split "\n"
        |> Array.map (fun row ->
            match row.Split "~" with
            | [| a; b; |] ->
                match Array.zip (a.Split ',' |> Array.map int) (b.Split ',' |> Array.map int) with
                | [| x; y; z; |] -> { x=x; y=y; z=z; }
                | _ -> failwith "Expected 3 fields on each side of brick definition"
            | _ -> failwith "Expected 2 sides to brick definition"
        )
        |> Array.toList

    let overlap b1 b2 =
        let b1x1, b1x2 = b1.x
        let b2x1, b2x2 = b2.x
        let b1y1, b1y2 = b1.y
        let b2y1, b2y2 = b2.y
        max b1x1 b2x1 <= min b1x2 b2x2 && max b1y1 b2y1 <= min b1y2 b2y2

    let settle bricks =
        bricks
        |> List.sortBy (fun b -> b.z)
        |> List.fold (fun acc b ->
            let bz1, bz2 = b.z
            let height = bz2 - bz1
            let lowerBrick =
                acc
                |> List.filter (fun b2 ->
                    let b2z1, b2z2 = b2.z
                    (max b2z1 b2z2) < (min bz1 bz2) && overlap b b2
                )
                |> List.sortByDescending (fun b2 -> max (b2.z |> fst) (b2.z |> snd))
                |> List.tryHead
            match lowerBrick with
            | Some b' ->
                let z1', z2' = b'.z
                let top = max z1' z2'
                let nb = { x=b.x; y=b.y; z=(top+1, top+1+height); }
                acc |> List.except [b] |> List.append [nb]
            | None ->
                // No lower bricks but needs to drop to ground
                match min bz1 bz2 > 1 with
                | true ->
                    let nb = { x=b.x; y=b.y; z=(1, 1+height); }
                    acc |> List.except [b] |> List.append [nb]
                | false -> acc
                
        ) bricks
    
    let getSupports bricks =
        bricks
        |> List.sortByDescending (fun b -> b.z)
        |> List.map (fun b ->
            let z1, z2 = b.z
            let supportingBricks =
                bricks
                |> List.filter (fun b2 ->
                    let z21, z22 = b2.z
                    ((max z1 z2) - (min z21 z22) = -1) && overlap b b2
                )
            (b, supportingBricks)
        )
        |> Map.ofList
    
    let getSupportedBy bricks =
        bricks
        |> List.sortByDescending (fun b -> b.z)
        |> List.map (fun b ->
            let z1, z2 = b.z
            let supportingBricks =
                bricks
                |> List.filter (fun b2 ->
                    let z21, z22 = b2.z
                    ((min z1 z2) - (max z21 z22) = 1) && overlap b b2
                )
            (b, supportingBricks)
        )
        |> Map.ofList

    let part1 (str : string) =
        let bricks = parseInput str |> settle
        let supports = bricks |> getSupports
        let supportedBy = bricks |> getSupportedBy
        let redundantBricks =
            bricks
            |> List.filter (fun b ->  
                supports
                |> Map.find b
                |> List.forall (fun b2 ->
                    (supportedBy |> Map.find b2).Length >= 2
                )
            )
        redundantBricks.Length

    let part2 (str : string, expansionFactor : int) =
        -1