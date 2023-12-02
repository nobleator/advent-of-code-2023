module Day01
    let rec window (str : string list) = 
        if str.Length = 0 then [] else
        if str.Length < 3 then [str.Head] @ window str.Tail else
        List.splitAt 3 str
        |> fun (x1, y1) ->
            match String.concat "" x1 with
            // Note: adding last letter for certain digits to handle overlapping case
            | "one" -> ["1"] @ window (["e"] @ y1)
            | "two" -> ["2"] @ window (["o"] @ y1)
            | "six" -> ["6"] @ window y1
            | _ ->
                if str.Length < 4 then [str.Head] @ window str.Tail else
                List.splitAt 4 str
                |> fun (x2, y2) ->
                    match String.concat "" x2 with
                    | "four" -> ["4"] @ window y2
                    | "five" -> ["5"] @ window (["e"] @ y2)
                    | "nine" -> ["9"] @ window (["e"] @ y2)
                    | _ ->
                        if str.Length < 5 then [str.Head] @ window str.Tail else
                        List.splitAt 5 str
                        |> fun (x3, y3) ->
                            match String.concat "" x3 with
                                | "three" -> ["3"] @ window (["e"] @ y3)
                                | "seven" -> ["7"] @ window (["n"] @ y3)
                                | "eight" -> ["8"] @ window (["t"] @ y3)
                                | _ -> [str.Head] @ window str.Tail

    let preProcess str  =
        str
        |> Seq.toList
        |> List.map string
        |> window
        |> String.concat ""
    
    let findFirstDigit str =
        Seq.pick (fun x -> if System.Char.IsDigit x then Some(x) else None) str

    let parseLine line =
        let newLine = preProcess line
        newLine
        |> Array.ofSeq
        |> Array.rev
        |> findFirstDigit
        |> fun x -> (findFirstDigit newLine).ToString() + x.ToString()
        |> int
    
    let calibrate (x : string) =
        x.Split [|'\n'|]
        |> Array.toList
        |> List.fold (fun total n -> total + parseLine n) 0
