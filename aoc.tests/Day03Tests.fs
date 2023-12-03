module Day03Tests

open Xunit

[<Fact>]
let ``Day 3-1 sample`` () =
    let x = @"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."
    let res = Day03.part1 x
    Assert.Equal(4361, res)

[<Fact>]
let ``Day 3-2 sample`` () =
    let x = @"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."
    let res = Day03.part2 x
    Assert.Equal(467835, res)
