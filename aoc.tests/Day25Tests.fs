module Day25Tests

open Xunit

[<Literal>]
let sample = @"jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr"

[<Fact>]
let ``Day 25-1 sample`` () =
    let res = Day25.part1 sample
    Assert.Equal(54, res)

[<Fact>]
let ``Day 25-1 cuts`` () =
    let res = Day25.part1 @"1: 2 3 4
2: 1 4 5
4: 1 2 3 5"
    Assert.Equal(-1, res)

[<Fact>]
let ``Day 25-2 sample`` () =
    let res = Day25.part2 sample
    Assert.Equal(0, res)
