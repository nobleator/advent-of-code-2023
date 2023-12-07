module Day07
    let parseInput (str : string) =
        str.Split [|'\n'|]
        |> Array.map (fun x ->
            match x.Split ' ' |> Array.toList with
            | [hand; bid] -> (hand, bid |> int)
            | _ -> failwith "Unexpected input row"
        )
        |> Array.toList

    let ord = ['2';'3';'4';'5';'6';'7';'8';'9';'T';'J';'Q';'K';'A';]

    let getStrength hand = 
        let cards = hand |> Seq.toList
        let cardCounts = 
            cards
            |> List.countBy id
            |> List.sortByDescending snd
        let handType =
            match cardCounts with
            | [ _; ] -> 6
            | [ a; _; ] when (a |> snd) = 4 -> 5
            | [ a; _; ] when (a |> snd) = 3 -> 4
            | [ a; _; _; ] when (a |> snd) = 3 -> 3
            | [ a; _; _; ] when (a |> snd) = 2 -> 2
            | [ _; _; _; _; ] -> 1
            | [ _; _; _; _; _; ] -> 0
            | _ -> failwith "Unexpected card count split"
        let ordinalRanks =
            cards
            |> List.map (fun c -> List.findIndex (fun x -> x = c) ord)
        (handType, ordinalRanks)

    let part1 (str : string) =
        parseInput str
        |> List.map (fun x -> (x, x |> fst |> getStrength))
        |> List.sortBy snd
        |> List.mapi (fun i x -> (i + 1) * (x |> fst |> snd))
        |> List.fold (fun acc v -> acc + v) 0

    let part2 (str : string) =
        0