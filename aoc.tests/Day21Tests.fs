module Day21Tests

open Xunit

[<Literal>]
let sample = @""

[<Fact>]
let ``Day 21-1 sample`` () =
    let res = Day21.part1 sample
    Assert.Equal(0, res)

[<Fact>]
let ``Day 21-2 sample`` () =
    let res = Day21.part2 (sample, 10)
    Assert.Equal(0, res)
