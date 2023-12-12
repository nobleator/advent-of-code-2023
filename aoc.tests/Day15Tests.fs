module Day15Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 15-1 sample`` () =
    let res = Day15.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 15-2 sample`` () =
    let res = Day15.part2 (sample, 10)
    Assert.Equal(0, res)
