module Day14Tests

open Xunit

[<Literal>]
let sample = @"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."

[<Fact>]
let ``Day 14-1 sample`` () =
    let res = Day14.part1 sample
    Assert.Equal(136, res)

[<Fact>]
let ``Day 14-2 sample`` () =
    let res = Day14.part2 sample
    Assert.Equal(64, res)
