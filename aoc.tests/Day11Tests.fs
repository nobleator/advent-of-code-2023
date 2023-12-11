module Day11Tests

open Xunit

[<Fact>]
let ``Day 11-1 sample 1`` () =
    let sample = @"...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."
    let res = Day11.part1 sample
    Assert.Equal(374, res)

// [<Fact>]
// let ``Day 11-2 sample`` () =
//     let res = Day11.part2 sample
//     Assert.Equal(2, res)