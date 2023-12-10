module Day10Tests

open Xunit

[<Fact>]
let ``Day 10-1 sample 1`` () =
    let sample = @"-L|F7
7S-7|
L|7||
-L-J|
L|-JF"
    let res = Day10.part1 sample
    Assert.Equal(4, res)

[<Fact>]
let ``Day 10-1 sample 2`` () =
    let sample = @"7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"
    let res = Day10.part1 sample
    Assert.Equal(8, res)

// [<Fact>]
// let ``Day 10-2 sample`` () =
//     let res = Day10.part2 sample
//     Assert.Equal(2, res)