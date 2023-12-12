module Day22Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 22-1 sample`` () =
    let res = Day22.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 22-2 sample`` () =
    let res = Day22.part2 (sample, 10)
    Assert.Equal(0, res)
