module Day18Tests

open Xunit

[<Literal>]
let sample = @"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"

[<Fact>]
let ``Day 18-1 sample`` () =
    let res = Day18.part1 sample
    Assert.Equal(62.0, res)

[<Fact>]
let ``Day 18-2 sample`` () =
    let res = Day18.part2 sample
    Assert.Equal(952408144115.0, res)
