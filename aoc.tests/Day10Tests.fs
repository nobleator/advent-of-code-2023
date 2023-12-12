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

[<Fact>]
let ``Day 10-2 sample 1`` () =
    let sample = @"...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........."
    let res = Day10.part2 sample
    Assert.Equal(4, res)

[<Fact>]
let ``Day 10-2 sample 2`` () =
    let sample = @"..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
.........."
    let res = Day10.part2 sample
    Assert.Equal(4, res)

[<Fact>]
let ``Day 10-2 sample 3`` () =
    let sample = @".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ..."
    let res = Day10.part2 sample
    Assert.Equal(8, res)

[<Fact>]
let ``Day 10-2 sample 4`` () =
    let sample = @"FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L"
    let res = Day10.part2 sample
    Assert.Equal(10, res)