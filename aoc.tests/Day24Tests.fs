module Day24Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 24-1 sample`` () =
    let res = Day24.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 24-2 sample`` () =
    let res = Day24.part2 (sample, 10)
    Assert.Equal(0, res)
