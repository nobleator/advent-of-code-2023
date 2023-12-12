module Day19Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 19-1 sample`` () =
    let res = Day19.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 19-2 sample`` () =
    let res = Day19.part2 (sample, 10)
    Assert.Equal(0, res)
