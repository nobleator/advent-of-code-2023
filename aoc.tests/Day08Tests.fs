module Day08Tests

open Xunit

// [<Literal>]
// let sample = @"RL

// AAA = (BBB, CCC)
// BBB = (DDD, EEE)
// CCC = (ZZZ, GGG)
// DDD = (DDD, DDD)
// EEE = (EEE, EEE)
// GGG = (GGG, GGG)
// ZZZ = (ZZZ, ZZZ)"

[<Fact>]
let ``Day 8-1 sample 1`` () =
    let sample = @"RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"
    let res = Day08.part1 sample
    Assert.Equal(2, res)

[<Fact>]
let ``Day 8-1 sample 2`` () =
    let sample = @"LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"
    let res = Day08.part1 sample
    Assert.Equal(6, res)

[<Fact>]
let ``Day 8-2 sample`` () =
    let sample = @"LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"
    let res = Day08.part2 sample
    Assert.Equal(6L, res)