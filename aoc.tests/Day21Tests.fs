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
let ``Day 21-2 sample 1`` () =
    let res = Day21.part2 sample 6L
    Assert.Equal(16L, res)

[<Fact>]
let ``Day 21-2 sample 2`` () =
    let res = Day21.part2 sample 10L
    Assert.Equal(50L, res)

[<Fact>]
let ``Day 21-2 sample 3`` () =
    let res = Day21.part2 sample 50L
    Assert.Equal(1594L, res)

[<Fact>]
let ``Day 21-2 sample 4`` () =
    let res = Day21.part2 sample 100L
    Assert.Equal(6536L, res)

[<Fact>]
let ``Day 21-2 sample 5`` () =
    let res = Day21.part2 sample 500L
    Assert.Equal(167004L, res)

[<Fact>]
let ``Day 21-2 sample 6`` () =
    let res = Day21.part2 sample 1000L
    Assert.Equal(668697L, res)

[<Fact>]
let ``Day 21-2 sample 7`` () =
    let res = Day21.part2 sample 5000L
    Assert.Equal(16733044L, res)
