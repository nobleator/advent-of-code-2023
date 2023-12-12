module Day25Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 25-1 sample`` () =
    let res = Day25.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 25-2 sample`` () =
    let res = Day25.part2 (sample, 10)
    Assert.Equal(0, res)
