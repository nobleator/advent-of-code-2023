module Day24
    type Line = { position: float * float * float; velocity: float * float * float; }

    let toFloatArray (str : string) =
        str.Split ','
        |> Array.filter (fun z -> not (System.String.IsNullOrWhiteSpace z))
        |> Array.map float

    let parseInput (str : string) =
        str.Split '\n'
        |> Array.map (fun row ->                    
            match row.Split '@' with
            | [| a; b; |] ->
                let position = toFloatArray a
                let velocity = toFloatArray b
                {
                    position=(position[0], position[1], position[2]);
                    velocity=(velocity[0], velocity[1], velocity[2]);
                }
            | _ -> failwith "Unexpected failed split"
        )

    let step line =
        let x, y, z = line.position
        let x', y', z' = line.velocity
        (x+x', y+y', z+z')

    let between p1 p2 p3 =
        // Checks if p1 is between p2 and p3
        let x1, y1, _ = p1
        let x2, y2, _ = p2
        let x3, y3, _ = p3
        ((x1 >= x2 && x1 <= x3) || (x1 >= x3 && x1 <= x2)) && 
        ((y1 >= y2 && y1 <= y3) || (y1 >= y3 && y1 <= y2))

    let lineToSlopeInterceptForm line =
        let x1, y1, _ = line.position
        let a, b, _ = line.velocity
        let m = float b / float a
        let c = float y1 - m * float x1
        (m, c)

    let intersectSlopes eq1 eq2 =
        // y = ax + b
        // intersection at y = a1 * x + b1 = a2 * x - b2
        let a1, b1 = eq1
        let a2, b2 = eq2
        let x = (b2 - b1) / (a1 - a2)
        let y = a1 * x + b1
        (x, y, 0.0)

    let futureIntersect line1 line2 =
        // if next step is between original point and intersection, then intersection is in the future 
        let x1, y1 = lineToSlopeInterceptForm line1
        let x2, y2 = lineToSlopeInterceptForm line2
        let i = intersectSlopes (x1, y1) (x2, y2)
        let next1 = step line1
        let next2 = step line2
        let inFuture = between next1 line1.position i && between next2 line2.position i
        (inFuture, i)
    
    let contained (minXY, maxXY) point =
        let x, y, _ = point
        minXY <= x && x <= maxXY && minXY <= y && y <= maxXY
    
    let permutations lst =
        let rec inner acc x =
            match x with
            | h::t -> acc @ inner (List.allPairs [h] t) t
            | [] -> acc
        inner [] lst

    let part1 (str : string) testArea =
        parseInput str
        |> Array.toList
        |> permutations
        |> List.map (fun (a, b) -> futureIntersect a b)
        |> List.filter (fun (a, b) -> a && contained testArea b)
        |> List.length

    let part2 (str : string) =
        -1