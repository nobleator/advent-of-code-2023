module Day17Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 17-1 sample`` () =
    let res = Day17.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 17-2 sample`` () =
    let res = Day17.part2 (sample, 10)
    Assert.Equal(0, res)
