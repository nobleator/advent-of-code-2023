module Day16Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 16-1 sample`` () =
    let res = Day16.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 16-2 sample`` () =
    let res = Day16.part2 (sample, 10)
    Assert.Equal(0, res)
