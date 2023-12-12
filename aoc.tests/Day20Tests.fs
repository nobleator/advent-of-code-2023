module Day20Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 20-1 sample`` () =
    let res = Day20.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 20-2 sample`` () =
    let res = Day20.part2 (sample, 10)
    Assert.Equal(0, res)
