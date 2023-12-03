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

// [<Fact>]
// let ``Day 2-2 sample`` () =
//     let x = @"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
// Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
// Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
// Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
// Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
//     let res = Day02.part2 x
//     Assert.Equal(2286, res)
