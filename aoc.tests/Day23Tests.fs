module Day23Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 23-1 sample`` () =
    let res = Day23.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 23-2 sample`` () =
    let res = Day23.part2 (sample, 10)
    Assert.Equal(0, res)
