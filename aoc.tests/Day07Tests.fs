module Day07Tests

open Xunit

[<Literal>]
let sample = @"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"

[<Fact>]
let ``Day 7-1 sample`` () =
    let res = Day07.part1 sample
    Assert.Equal(6440, res)

[<Fact>]
let ``Day 7-2 sample`` () =
    let res = Day07.part2 sample
    Assert.Equal(5905, res)

[<Fact>]
let ``Day 7-2 all jokers`` () =
    let res = Day07.part2 @"JJJJJ 10
22222 20"
    Assert.Equal(50, res)