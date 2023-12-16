module Day15Tests

open Xunit

[<Literal>]
let sample = @"rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

[<Fact>]
let ``Day 15-1 sample`` () =
    let res = Day15.part1 sample
    Assert.Equal(1320, res)

[<Fact>]
let ``Day 15-1 sample 2`` () =
    let res = Day15.part1 "HASH"
    Assert.Equal(52, res)

[<Fact>]
let ``Day 15-2 sample`` () =
    let res = Day15.part2 sample
    Assert.Equal(145, res)
