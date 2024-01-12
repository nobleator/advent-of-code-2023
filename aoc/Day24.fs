module Day24
    open MathNet.Numerics.LinearAlgebra

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

    let calculateIntercept hailstones =
        match hailstones with
        | [hs; hs'; hs'';] ->
            let x, y, z = hs.position
            let dx, dy, dz = hs.velocity
            let x', y', z' = hs'.position
            let dx', dy', dz' = hs'.velocity
            let x'', y'', z'' = hs''.position
            let dx'', dy'', dz'' = hs''.velocity
            let A = matrix [
                [ (dy'-dy);  (dx-dx'); 0;  (y-y');  (x'-x); 0;];
                [(dy''-dy); (dx-dx''); 0; (y-y''); (x''-x); 0;];
                [ (dz'-dz); 0;  (dx-dx');  (z-z'); 0;  (x'-x);];
                [(dz''-dz); 0; (dx-dx''); (z-z''); 0; (x''-x);];
                [0;  (dz'-dz);  (dy-dy'); 0;  (z-z');  (y'-y);];
                [0; (dz''-dz); (dy-dy''); 0; (z-z''); (y''-y);];
            ]
            let b = vector [
                x'*dy' - y'*dx' - x*dy + y*dx;
                x''*dy'' - y''*dx'' - x*dy + y*dx;
                x'*dz' - z'*dx' - x*dz + z*dx;
                x''*dz'' - z''*dx'' - x*dz + z*dx;
                y'*dz' - z'*dy' - y*dz + z*dy;
                y''*dz'' - z''*dy'' - y*dz + z*dy;
            ]
            let x' = A.Solve b
            x'
        | _ -> failwith "3 hailstones expected"

    let rec mitigate hailstones =
        // To mitigate floating point errors, re-run a few times
        let rec inner offset lastEstimate hs =
            let vec =
                hs
                |> List.skip offset
                |> List.take 3
                |> calculateIntercept
            match vec.ToArray() with
            | [| x; y; z; _; _; _; |] ->
                let total = x+y+z |> int64
                match total <> lastEstimate with
                | true -> inner (offset+3) total hailstones
                | false -> total
            | _ -> failwith ""
        inner 0 0L hailstones

    let part1 (str : string) testArea =
        parseInput str
        |> Array.toList
        |> permutations
        |> List.map (fun (a, b) -> futureIntersect a b)
        |> List.filter (fun (a, b) -> a && contained testArea b)
        |> List.length

    let part2 (str : string) =
        (*
            inspired by https://old.reddit.com/r/adventofcode/comments/18q40he/2023_day_24_part_2_a_straightforward_nonsolver/

            given position and velocity vectors for 3 hailstones:
            (x, y, z) @ (dx, dy, dz)
            (x', y', z') @ (dx', dy', dz')
            (x'', y'', z'') @ (dx'', dy'', dz'')

            and given all 3 hailstones are guaranteed to intersect with a rock of position and velocity vectors:
            (X, Y, Z) @ (DX, DY, DZ)

            as we are looking for a starting position and velocity in 3D space, we have 6 unknown variables: X, Y, Z, DX, DY, DZ
            if we can derive 6 equations, we can solve for the 6 unknowns easily using linear algebra.

            for a single hailstone and rock, the formula for intersection including time (t) is:
            X + t DX = x + t dx

            refactored to solve for t:
            t = (X - x) / (dx - DX)

            for the same t we would also intersect in the y and z coordinates, giving us:
            t = (X - x) / (dx - DX)
            t = (Y - y) / (dy - DY)
            t = (Z - z) / (dz - DZ)

            given the common t, we can set any of these equations equal to each other. starting with x & y:
            (X - x) / (dx - DX) = (Y - y) / (dy - DY)
            => Y DX - X DY = x dy - y dx + Y dx + y DX - x DY - X dy

            repeating for x & z:
            (X - x) / (dx - DX) = (Z - z) / (dz - DZ)
            => Z DX - X DZ = x dz - z dx + Z dx + z DX - x DZ - X dz

            repeating for y & z:
            (X - x) / (dx - DX) = (Z - z) / (dz - DZ)
            => Z DY - Y DZ = y dz - z dy + Z dy + z DY - y DZ - Y dz

            since the left hand side (LHS) of these equations are entirely referencing the rock variables, not the hailstones, we can repeat for each hailstone and then set the equations equal to each other. for hailstones (x, y, z) and (x', y', z'), solving for x & y:
            x dy - y dx + Y dx + y DX - x DY - X dy = x' dy' - y' dx' + Y dx' + y' DX - x' DY - X dy'
            => (dy'-dy) X + (dx-dx') Y + (y-y') DX + (x'-x) DY = x' dy' - y' dx' - x dy + y dx

            repeating for (x, y, z) and (x'', y'', z''), still solving for x & y, gives us:
            (dy''-dy) X + (dx-dx'') Y + (y-y'') DX + (x''-x) DY = x'' dy'' - y'' dx'' - x dy + y dx

            we can now repeat this process with (x, y, z) and (x', y', z'), solving for x & z:
            x dz - z dx + Z dx + z DX - x DZ - X dz = x' dz' - z' dx' + Z dx' + z' DX - x' DZ - X dz'
            => (dz'-dz) X + (dx-dx') Z + (z-z') DX + (x'-x) DZ = x' dz' - z' dx' - x dz + z dx

            and (x, y, z) and (x'', y'', z''), solving for x & z:
            (dz''-dz) X + (dx-dx'') Z + (z-z'') DX + (x''-x) DZ = x'' dz'' - z'' dx'' - x dz + z dx

            (x, y, z) and (x', y', z'), solving for y & z:
            y dz - z dy + Z dy + z DY - y DZ - Y dz = y' dz' - z' dy' + Z dy' + z' DY - y' DZ - Y dz'
            => (dz'-dz) Y + (dy-dy') Z + (z-z') DY + (y'-y) DZ = y' dz' - z' dy' - y dz + z dy

            and finally (x, y, z) and (x'', y'', z''), solving for y & z:
            (dz''-dz) Y + (dy-dy'') Z + (z-z'') DY + (y''-y) DZ = y'' dz'' - z'' dy'' - y dz + z dy

            as we know values for all of the hailstones (the lowercase variables), this gives us 6 equations and 6 unknowns (X, Y, Z, DX, DY, DZ):
            (dy'-dy) X + (dx-dx') Y + (y-y') DX + (x'-x) DY = x' dy' - y' dx' - x dy + y dx
            (dy''-dy) X + (dx-dx'') Y + (y-y'') DX + (x''-x) DY = x'' dy'' - y'' dx'' - x dy + y dx
            (dz'-dz) X + (dx-dx') Z + (z-z') DX + (x'-x) DZ = x' dz' - z' dx' - x dz + z dx
            (dz''-dz) X + (dx-dx'') Z + (z-z'') DX + (x''-x) DZ = x'' dz'' - z'' dx'' - x dz + z dx
            (dz'-dz) Y + (dy-dy') Z + (z-z') DY + (y'-y) DZ = y' dz' - z' dy' - y dz + z dy
            (dz''-dz) Y + (dy-dy'') Z + (z-z'') DY + (y''-y) DZ = y'' dz'' - z'' dy'' - y dz + z dy

            we can tidy this up a bit to form a matrix of the form Ax = b:
             (dy'-dy) X +  (dx-dx') Y +         0 Z +  (y-y') DX +  (x'-x) DY +       0 DZ = x' dy' - y' dx' - x dy + y dx
            (dy''-dy) X + (dx-dx'') Y +         0 Z + (y-y'') DX + (x''-x) DY +       0 DZ = x'' dy'' - y'' dx'' - x dy + y dx
             (dz'-dz) X +         0 Y +  (dx-dx') Z +  (z-z') DX +       0 DY +  (x'-x) DZ = x' dz' - z' dx' - x dz + z dx
            (dz''-dz) X +         0 Y + (dx-dx'') Z + (z-z'') DX +       0 DY + (x''-x) DZ = x'' dz'' - z'' dx'' - x dz + z dx
                    0 X +  (dz'-dz) Y +  (dy-dy') Z +       0 DX +  (z-z') DY +  (y'-y) DZ = y' dz' - z' dy' - y dz + z dy
                    0 X + (dz''-dz) Y + (dy-dy'') Z +       0 DX + (z-z'') DY + (y''-y) DZ = y'' dz'' - z'' dy'' - y dz + z dy

            this gives us a matrix A:
            [
                [ (dy'-dy),  (dx-dx'), 0,  (y-y'),  (x'-x), 0],
                [(dy''-dy), (dx-dx''), 0, (y-y''), (x''-x), 0],
                [ (dz'-dz), 0,  (dx-dx'),  (z-z'), 0,  (x'-x)],
                [(dz''-dz), 0, (dx-dx''), (z-z''), 0, (x''-x)],
                [0,  (dz'-dz),  (dy-dy'), 0,  (z-z'),  (y'-y)],
                [0, (dz''-dz), (dy-dy''), 0, (z-z''), (y''-y)]
            ]

            and a vector x:
            [
                X,
                Y,
                Z,
                DX,
                DY,
                DZ
            ]

            and a vector b:
            [
                x' dy' - y' dx' - x dy + y dx,
                x'' dy'' - y'' dx'' - x dy + y dx,
                x' dz' - z' dx' - x dz + z dx,
                x'' dz'' - z'' dx'' - x dz + z dx,
                y' dz' - z' dy' - y dz + z dy,
                y'' dz'' - z'' dy'' - y dz + z dy
            ]

            we can now solve this matrix any way we want.
        *)
        parseInput str |> Array.toList |> mitigate