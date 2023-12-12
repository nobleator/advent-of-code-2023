module Day18Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 18-1 sample`` () =
    let res = Day18.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 18-2 sample`` () =
    let res = Day18.part2 (sample, 10)
    Assert.Equal(0, res)
