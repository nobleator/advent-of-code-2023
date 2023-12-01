module Day01
    let findFirstDigit str =
        Seq.pick (fun x -> if System.Char.IsDigit x then Some(x) else None) str

    let parseLine (line: string) =
        line
        |> Array.ofSeq
        |> Array.rev
        |> findFirstDigit
        |> fun x -> (findFirstDigit line).ToString() + x.ToString()
        |> int
    
    let calibrate (x : string) =
        x.Split [|'\n'|]
        |> Array.toList
        |> List.fold (fun total n -> total + parseLine n) 0
