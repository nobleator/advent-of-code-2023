module Day12Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 12-1 sample`` () =
    let res = Day12.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 12-2 sample`` () =
    let res = Day12.part2 (sample, 10)
    Assert.Equal(0, res)
