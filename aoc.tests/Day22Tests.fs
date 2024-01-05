module Day22Tests

open Xunit
open Day22

[<Literal>]
let sample = @"1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9"

[<Fact>]
let ``Day 22-1 sample`` () =
    let res = Day22.part1 sample
    Assert.Equal(5, res)

[<Fact>]
let ``Day 22-1 settle to ground`` () =
    let bricks = Day22.parseInput @"0,0,2~1,0,2"
    let bricks' = bricks |> Day22.settle
    let exp = [{ x=(0,1); y=(0,0); z=(1,1); } ]
    Assert.Equivalent(exp, bricks')

[<Fact>]
let ``Day 22-2 sample`` () =
    let res = Day22.part2 sample
    Assert.Equal(7, res)
