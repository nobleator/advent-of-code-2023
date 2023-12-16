module Day16Tests

open Xunit

[<Literal>]
let sample = @".|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|...."

[<Fact>]
let ``Day 16-1 sample`` () =
    let res = Day16.part1 sample
    Assert.Equal(46, res)

[<Fact>]
let ``Day 16-1 cycle`` () =
    let res = Day16.part1 @"..|..
./-\.
.....
.\./."
    Assert.Equal(11, res)

[<Fact>]
let ``Day 16-1 simple split`` () =
    let res = Day16.part1 @"..|..
.....
..-.."
    Assert.Equal(9, res)

[<Fact>]
let ``Day 16-1 diagonal`` () =
    let res = Day16.part1 @"...\...
.......
-......
.......
\../..."
    Assert.Equal(18, res)

[<Fact>]
let ``Day 16-1 first mirror cycle`` () =
    let res = Day16.part1 @"|....-
......
......
-....|"
    Assert.Equal(16, res)

[<Fact>]
let ``Day 16-1 overlapping cycle`` () =
    let res = Day16.part1 @"......|...\..\...
..../........|...
....\.-.../......
......|....../...
................."
    Assert.Equal(41, res)

[<Fact>]
let ``Day 16-2 sample`` () =
    let res = Day16.part2 sample
    Assert.Equal(0, res)
