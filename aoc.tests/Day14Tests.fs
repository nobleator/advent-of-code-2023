module Day14Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 14-1 sample`` () =
    let res = Day14.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 14-2 sample`` () =
    let res = Day14.part2 (sample, 10)
    Assert.Equal(0, res)
