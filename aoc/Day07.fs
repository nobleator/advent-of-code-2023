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
    let ord2 = ['J';'2';'3';'4';'5';'6';'7';'8';'9';'T';'Q';'K';'A';]

    let getHandTypeStrength cards =
        let cardCounts = 
            cards
            |> List.countBy id
            |> List.sortByDescending snd
        match cardCounts with
        | [ _; ] -> 6
        | [ a; _; ] when (a |> snd) = 4 -> 5
        | [ a; _; ] when (a |> snd) = 3 -> 4
        | [ a; _; _; ] when (a |> snd) = 3 -> 3
        | [ a; _; _; ] when (a |> snd) = 2 -> 2
        | [ _; _; _; _; ] -> 1
        | [ _; _; _; _; _; ] -> 0
        | _ -> failwith "Unexpected card count split"

    let getRankStrength rankList cards =
        cards |> List.map (fun c -> List.findIndex (fun x -> x = c) rankList)

    let getStrength hand = 
        let cards = hand |> Seq.toList
        (getHandTypeStrength cards, getRankStrength ord cards)

    let getStrengthWilds (hand : string) = 
        let cards = hand |> Seq.toList
        let wilds =
            cards
            |> List.map (fun c -> hand.Replace('J', c))
            |> List.map (fun x -> x |> Seq.toList |> getHandTypeStrength)
        (List.max wilds, getRankStrength ord2 cards)

    let part1 (str : string) =
        parseInput str
        |> List.map (fun x -> (x, x |> fst |> getStrength))
        |> List.sortBy snd
        |> List.mapi (fun i x -> (i + 1) * (x |> fst |> snd))
        |> List.fold (fun acc v -> acc + v) 0

    let part2 (str : string) =
        parseInput str
        |> List.map (fun x -> (x, x |> fst |> getStrengthWilds))
        |> List.sortBy snd
        |> List.mapi (fun i x -> (i + 1) * (x |> fst |> snd))
        |> List.fold (fun acc v -> acc + v) 0