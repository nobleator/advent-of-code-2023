module Day21Tests

open Xunit

[<Literal>]
let sample = @"...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........."

[<Fact>]
let ``Day 21-1 sample`` () =
    let res = Day21.part1 sample 6
    Assert.Equal(16, res)

[<Fact>]
let ``Day 21-1 subsample 1`` () =
    let res = Day21.part1 sample 1
    Assert.Equal(2, res)

[<Fact>]
let ``Day 21-1 subsample 2`` () =
    let res = Day21.part1 sample 2
    Assert.Equal(4, res)

[<Fact>]
let ``Day 21-1 subsample 3`` () =
    let res = Day21.part1 sample 3
    Assert.Equal(6, res)

[<Fact>]
let ``Day 21-2 sample`` () =
    let res = Day21.part2 sample
    Assert.Equal(0, res)
