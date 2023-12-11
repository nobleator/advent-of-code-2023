module Day11Tests

open Xunit

[<Literal>]
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

[<Fact>]
let ``Day 11-1 sample`` () =
    let res = Day11.part1 sample
    Assert.Equal(374, res)

[<Fact>]
let ``Day 11-2 sample 1`` () =
    let res = Day11.part2 (sample, 10)
    Assert.Equal(1030L, res)

[<Fact>]
let ``Day 11-2 sample 2`` () =
    let res = Day11.part2 (sample, 100)
    Assert.Equal(8410L, res)