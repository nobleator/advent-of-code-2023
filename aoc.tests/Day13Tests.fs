module Day13Tests

open Xunit

[<Literal>]
let sample = @"#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"

[<Fact>]
let ``Day 13-1 sample`` () =
    let res = Day13.part1 sample
    Assert.Equal(405, res)

// [<Fact>]
// let ``Day 13-2 sample`` () =
//     let res = Day13.part2 (sample, 10)
//     Assert.Equal(0, res)
