module Day09Tests

open Xunit

[<Literal>]
let sample = @"0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"

[<Fact>]
let ``Day 9-1 sample`` () =
    let res = Day09.part1 sample
    Assert.Equal(114, res)

[<Fact>]
let ``Day 9-2 sample`` () =
    let res = Day09.part2 sample
    Assert.Equal(2, res)