module Day17Tests

open Xunit

[<Literal>]
let sample = @"2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"

[<Fact>]
let ``Day 17-1 sample`` () =
    let res = Day17.part1 sample
    Assert.Equal(102, res)

[<Fact>]
let ``Day 17-2 sample`` () =
    let res = Day17.part2 sample
    Assert.Equal(0, res)
