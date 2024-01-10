module Day24
    type Line = { position: float * float * float; velocity: float * float * float; }

    // TODO: is float sufficient for large inputs?
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

    let crossProduct v u =
        let (v1, v2, v3) = v
        let (u1, u2, u3) = u
        (v2*u3-v3*u2, v3*u1-v1*u3, v1*u2-v2*u1)

    let normalize vector =
        let x, y, z = vector
        sqrt (x*x+y*y+z*z)

    let intersect line1 line2 =
        // https://math.stackexchange.com/a/271366
        let (x1, y1, z1) = line1.position
        let (x2, y2, z2) = line2.position
        let g = (x2-x1, y2-y1, z2-z1)
        let cx, cy, cz = line1.position
        // let d = line2.position
        let e = line1.velocity
        let f = line2.velocity
        let ex, ey, ez = e
        let fg = crossProduct f g
        let fe = crossProduct f e
        let coefficient = normalize fg / normalize fe
        let M = (cx+coefficient*ex, cy+coefficient*ey, cz+coefficient*ez)
        M

    let removeZ elem =
        let x, y, _ = elem
        (x, y, 0.0)

    let intersectXY line1 line2 =
        let line1' = { position=(removeZ line1.position); velocity=(removeZ line1.velocity); }
        let line2' = { position=(removeZ line2.position); velocity=(removeZ line2.velocity); }
        intersect line1' line2'

    let step line =
        let x, y, z = line.position
        let x', y', z' = line.velocity
        (x+x', y+y', z+z')

    let between p1 p2 p3 =
        // Checks if p1 is between p2 and p3
        let x1, y1, z1 = p1
        let x2, y2, z2 = p2
        let x3, y3, z3 = p3
        ((x1 >= x2 && x1 <= x3) || (x1 >= x3 && x1 <= x2)) && 
        ((y1 >= y2 && y1 <= y3) || (y1 >= y3 && y1 <= y2)) && 
        ((z1 >= z2 && z1 <= z3) || (z1 >= z3 && z1 <= z2))

    let futureIntersect intersectFun line1 line2 =
        // if next step is between original point and intersection, then intersection is in the future 
        let i = intersectFun line1 line2
        let inFuture = between (step line1) line1.position i && between (step line2) line2.position i
        (inFuture, i)
    
    let contained (minXY, maxXY) point =
        let x, y, z = point
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
        |> List.map (fun (a, b) -> futureIntersect intersectXY a b)
        |> List.filter (fun (a, b) -> a && contained testArea b)
        |> List.length

    let part2 (str : string) =
        -1