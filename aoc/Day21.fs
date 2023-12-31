module Day21
    open Day20

    type BigPoint = { x: int64; y: int64}

    let parseInput (str : string) =
        str.Split "\n"
        |> Array.map Seq.toList
        |> Array.mapi (fun yIdx r -> (yIdx, r))
        |> Array.fold (fun rowAcc row ->
            let yIdx, r = row
            let tmp =
                r
                |> List.mapi (fun xIdx c -> (xIdx, c))
                |> List.fold (fun colAcc col ->
                    let xIdx, c = col
                    colAcc @ [({x=xIdx; y=yIdx}, c)]
                ) []
            rowAcc @ tmp
        ) []

    let nextEdge dirt edge  seen width height =
        edge
        |> Set.toList
        |> List.collect (fun e ->
            [(0L,1L); (0L,-1L); (1L,0L); (-1L,0L)] |> List.map (fun (nx, ny) ->
                {x=e.x + nx; y=e.y + ny}
            )
        )
        |> List.filter (fun p ->
            let mx = if p.x < 0 then (3L * width + p.x) % width else p.x % width
            let my = if p.y < 0 then (3L * height + p.y) % height else p.y % height
            let mirroredPoint = {x=mx; y=my}
            Set.contains mirroredPoint dirt && not (Set.contains p seen)
        )
        |> Set.ofList

    let takeStep dirt edge seen width height = 
        let edge' = nextEdge dirt edge seen width height
        let seen' = Set.union seen edge'
        (edge', seen')

    let walk (str : string) goal width height =
        let start =
            parseInput str
            |> List.filter (fun x -> x |> snd = 'S')
            |> List.exactlyOne
            |> fst
        let _, dirt =
            parseInput str
            |> Set.ofList
            |> Set.partition (fun x -> x |> snd = '#')
        let dirt' = dirt |> Set.map fst
        let results = [(0, Set.empty); (1, Set.empty);] |> Map.ofList
        [0..goal]
        |> List.fold (fun acc i ->
            let (parity, dirt, edges, results) = acc
            let seen = Map.find parity results
            let e', s' = takeStep dirt edges seen width height
            let r' = addOrUpdate parity s' results
            let p' = 1-parity
            (p', dirt, e', r')
        ) (1, dirt', Set.singleton start, results)
        |> fun (_,_,_,r) -> Map.find (goal % 2) r |> Set.count
    
    let part1 (str : string) goal =
        let input = str.Split "\n"
        let height = input |> Array.length |> int64
        let width = input |> Array.head |> String.length |> int64
        walk str goal width height

    let part2 (str : string) goal =
        let input = str.Split "\n"
        let height = input |> Array.length |> int64
        let width = input |> Array.head |> String.length |> int64
        let res =
            [ goal % height; goal % height + height; goal % height + (height * 2L); ]
            |> List.map (fun x -> (walk str (x |> int) width height) |> int64)
        match res with
        | [ s'; s''; s''' ] ->
            // Magic interpolation formula
            let m = s'' - s'
            let n = s''' - s''
            let a = (n - m) / 2L
            let b = m - 3L * a
            let c = s' - b - a
            let ceiling = ceil ((goal |> float) / (height |> float))
            a * (ceiling**2.0 |> int64) + b * (ceiling |> int64) + c
        | _ -> failwith "Expected 3 results"