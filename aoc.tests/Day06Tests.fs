module Day06Tests

open Xunit

[<Literal>]
let sample = @"Time:      7  15   30
Distance:  9  40  200"

[<Fact>]
let ``Day 6-1 sample`` () =
    let res = Day06.part1 sample
    Assert.Equal(288, res)

// [<Fact>]
// let ``Day 6-2 sample`` () =
//     let res = Day06.part2 sample
//     Assert.Equal(-1, res)
